import Debug.Trace (trace)
import Data.Maybe
import Data.List
import Data.Ord
import Control.Monad.State
import Data.IntMap (IntMap)
import System.Environment (getArgs)

type Grid = [[Maybe Int]]

main = do
  args <- getArgs
  let n = read (head args) :: Int
  let f x = sequence $ map addValue [1..x]
  putStrLn $ evalState (do f n; prettyPrint) initGrid
  
initGrid :: Grid
initGrid = [[Just 0]]

prettyPrint :: State Grid String
prettyPrint = do
  g <- get
  let w = length (head g)
  let h = length g
  return $ concat $ "%!\n\n" : [prettyPrint' h w x y g | x <- [0..(h-1)], y <- [0..(w-1)]] ++ ["\nshowpage\n"]
prettyPrint' h w x y g =
  "newpath\n" ++
  x1 ++ " " ++ y1 ++ " moveto\n" ++
  x2 ++ " " ++ y1 ++ " lineto\n" ++
  x2 ++ " " ++ y2 ++ " lineto\n" ++
  x1 ++ " " ++ y2 ++ " lineto\n" ++
  "closepath\n" ++
  c ++ " setrgbcolor\nfill\n\n"
    where
      x1 = show $ min 573 $ (h - 1 - x) * 5 + 18
      y1 = show $ min 816 $ y * 5 + 21
      x2 = show $ min 573 $ (h - 1 - x) * 5 + 28
      y2 = show $ min 816 $ y * 5 + 31
      c  = if g!!x!!y == Nothing then "1.0 1.0 1.0" else "0.0 0.0 0.0"

expandGrid :: State Grid ()
expandGrid = do
  addTopRow
  addBottomRow
  addLeftColumn
  addRightColumn

addTopRow :: State Grid ()
addTopRow = state $ \(g:gs) ->
  ((), if catMaybes g == []
        then g : gs
        else (take (length g) nothings) : g : gs)

addBottomRow :: State Grid ()
addBottomRow = state $ \gs ->
  ((), if catMaybes (last gs) == []
        then gs
        else gs ++ [take (length (last gs)) nothings])

addLeftColumn :: State Grid ()
addLeftColumn = state $ \gs ->
  ((), if catMaybes (head (transpose gs)) == []
        then gs
        else let gx = transpose gs
                 h = length (head gx) in transpose $ (take h nothings) : gx)

addRightColumn :: State Grid ()
addRightColumn = state $ \gs ->
  ((), if catMaybes (last (transpose gs)) == []
        then gs
        else let gx = transpose gs
                 h = length (last gx) in transpose $ gx ++ [take h nothings])

nothings = repeat Nothing

calcValue :: Int -> Int -> State Grid Float
calcValue x y = state $ \g -> (calcValue' x y g, g)

calcValue' x y g
  | x < 0 || y < 0 || x >= length g || y >= length (head g) = fromIntegral (maxBound :: Int)
  | g!!x!!y /= Nothing = fromIntegral (maxBound :: Int)
  | unUsed (x+1) y && unUsed (x-1) y && unUsed x (y+1) && unUsed x (y-1) = fromIntegral (maxBound :: Int)
  | otherwise = sum ns' + (0.1 * (fromIntegral (length ns')))
      where
        unUsed x' y'
          | x' < 0 || y' < 0 || x' >= length g || y' >= length (head g) = True
          | otherwise = case g!!x'!!y' of Nothing  -> True
                                          (Just n) -> False
        neighborVal x' y'
          | x' < 0 || y' < 0 || x' >= length g || y' >= length (head g) = 0
          | otherwise = case g!!x'!!y' of Nothing  -> 0
                                          (Just n) -> n
        ns = [neighborVal (x+2) y, neighborVal (x-2) y, neighborVal x (y+2), neighborVal x (y-2),
              neighborVal (x+1) (y+1), neighborVal (x-1) (y-1), neighborVal (x+1) (y-1), neighborVal (x-1)
              (y+1)]
        ns' = map fromIntegral $ filter (/=0) ns

addValue :: Int -> State Grid ()
addValue n = do
  expandGrid
  state $ \g -> ((), addValue' n g)

addValue' :: Int -> Grid -> Grid
addValue' n g = t ++ (l ++ (Just n) : (tail r)) : (tail b)
  where
    w = length (head g)
    h = length g
    costList n
      | n == 0 = [((x,y), calcValue' x y g) | x <- [0..(h-1)],       y <- [0..(w-1)]]
      | n == 1 = [((x,y), calcValue' x y g) | x <- [(h-1),(h-2)..0], y <- [0..(w-1)]]
      | n == 2 = [((x,y), calcValue' x y g) | x <- [0..(h-1)],       y <- [(w-1),(w-2)..0]]
      | n == 3 = [((x,y), calcValue' x y g) | x <- [(h-1),(h-2)..0], y <- [(w-1),(w-2)..0]]
    (x,y) = fst $ minimumBy (comparing snd) (costList (n `mod` 4))
    (t,b) = splitAt x g
    (l,r) = splitAt y $ head b


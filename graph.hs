import Debug.Trace (trace)
import qualified Data.ByteString as B
import Data.Maybe
import Data.List
import Data.Ord
import Data.Map.Strict (Map)
import qualified Data.Map as Map
import Control.Monad.State
import System.Environment (getArgs)
import Data.Word
import Data.Bits
import qualified Data.Set as Set

type Point = (Int,Int)
type Value = Int
type Frontier = Map Point Value

main = do
  args <- getArgs
  mainArgs args

mainArgs (file:[]) = do
  contents <- B.readFile file
  let bs = B.unpack contents
  let scale = scaleCalc $ length bs
  let t1 = foldM (addValue scale) [(0,0)] $ tail bs
  let out = (0,0) : evalState t1 initFrontier
  putStrLn $ printPS scale bs out
  --print $ ((length out), (length (Set.toList (Set.fromList out))))
  --print $ runState t1 initFrontier
mainArgs _ = putStrLn "Invalid args"
  
initFrontier :: Frontier
initFrontier = Map.fromList $ zip initPoints $ repeat 0

addValue :: Float -> [Point] -> Word8 -> State Frontier [Point]
addValue scale ps b = do
  xs <- get
  let n = (length ps) - 1
  let b' = fromIntegral b
  let ls = Map.toList xs
  let best = minimumBy (comparing snd) ls
  let options = [fst i | i <- ls, (snd i) == (snd best)]
  let p@(x,y) = head $ drop b' $ cycle $
                  if odd b
                    then options
                    else reverse options
  put $ Map.delete p xs
  let newPoints' = filter (`notElem` ps) $ newPoints scale x y
  --sequence $ map (\s -> modify $ Map.insertWith (+) s ((x^2+y^2)*(b'-92))) newPoints'
  --sequence $ map (\s -> modify $ Map.insertWith (+) s ((b'-64))) newPoints'
  sequence $ map (\s -> modify $ Map.insertWith (+) s ((b'-127)^2)) newPoints'
  return (p:ps)

initPoints :: [Point]
initPoints = newPoints 1.0 0 0
newPoints :: Float -> Int -> Int -> [Point]
newPoints scale x y = filter (\(a,b) -> (abs (a*(round scale))) < round pp_cX
                        && (abs (b*(round scale))) < round pp_cY)
                        [(x-2,y+2),(x-1,y+2),(x+0,y+2),(x+1,y+2),(x+2,y+2),
                         (x-2,y+1),(x-1,y+1),(x+0,y+1),(x+1,y+1),(x+2,y+1),
                         (x-2,y+0),(x-1,y+0),          (x+1,y+0),(x+2,y+0),
                         (x-2,y-1),(x-1,y-1),(x+0,y-1),(x+1,y-1),(x+2,y-1),
                         (x-2,y-2),(x-1,y-2),(x+0,y-2),(x+1,y-2),(x+2,y-2)]

printPS :: Float -> [Word8] -> [Point] -> String
printPS scale bs ps =
  let
    s = show scale
    h = show $ scale / 2.0
  in
    "%!\n\n" ++
    "/drawcircle { newpath\n" ++
    "moveto\n" ++
    h ++ " 0.0 rmoveto\n" ++
    h ++ " 0.0 360.0 arc\n" ++
    "closepath\n" ++
    "0.5 0.5 sethsbcolor\nfill } def\n\n" ++
    concatMap (pp scale) (zip (reverse ps) bs) ++ "\nshowpage\n"

pp scale ((x,y), v) = h ++ " " ++ x' ++ " " ++ y' ++ " " ++ x' ++ " " ++ y' ++ " drawcircle\n"
  where
    x' = show $ pp_cX + scale * (fromIntegral x)
    y' = show $ pp_cY + scale * (fromIntegral y)
    h  = show $ (fromIntegral v) / 255.0

pp_cX :: Fractional a => a
pp_cX = 612.0 / 2.0
pp_cY :: Fractional a => a
pp_cY = 792.0 / 2.0

scaleCalc :: (Floating a, Integral b) => b -> a
scaleCalc s = (/) (576.0 / 2.0) $ sqrt $ fromIntegral s

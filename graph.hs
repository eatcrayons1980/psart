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

type Point = (Int,Int)
type Value = Int
type Frontier = Map Point Value

main = do
  args <- getArgs
  mainArgs args

mainArgs (file:[]) = do
  contents <- B.readFile file
  let bs = B.unpack contents
  let t1 = foldM addValue [(0,0)] bs
  putStrLn $ prettyPrint bs $ (0,0) : evalState t1 initFrontier
  --print $ runState t1 initFrontier
mainArgs _ = putStrLn "Invalid args"
  
initFrontier :: Frontier
initFrontier = Map.fromList $ zip initPoints $ repeat 0

addValue :: [Point] -> Word8 -> State Frontier [Point]
addValue ps b = do
  xs <- get
  let n = (length ps) - 1
  let b' = fromIntegral b
  let ls = Map.toList xs
  let best = minimumBy (comparing snd) ls
  let p@(x,y) = if odd b
                  then head $ drop b' $ cycle [fst i | i <- ls, (snd i) == (snd best)]
                  else head $ drop b' $ cycle $ reverse [fst i | i <- ls, (snd i) == (snd best)]
  put $ Map.delete p xs
  let newPoints' = filter (`notElem` ps) $ newPoints x y
  --sequence $ map (\s -> modify $ Map.insertWith (+) s ((x^2+y^2)*(b'-92))) newPoints'
  --sequence $ map (\s -> modify $ Map.insertWith (+) s ((b'-64))) newPoints'
  sequence $ map (\s -> modify $ Map.insertWith (+) s ((b'-127)^2)) newPoints'
  return (p:ps)

initPoints :: [Point]
initPoints = newPoints 0 0
newPoints :: Int -> Int -> [Point]
newPoints x y = filter (\(a,b) -> (abs (a*(round pp_scale))) < round pp_cX
                  && (abs (b*(round pp_scale))) < round pp_cY)
                  [(x-2,y+2),(x-1,y+2),(x+0,y+2),(x+1,y+2),(x+2,y+2),
                   (x-2,y+1),(x-1,y+1),(x+0,y+1),(x+1,y+1),(x+2,y+1),
                   (x-2,y+0),(x-1,y+0),          (x+1,y+0),(x+2,y+0),
                   (x-2,y-1),(x-1,y-1),(x+0,y-1),(x+1,y-1),(x+2,y-1),
                   (x-2,y-2),(x-1,y-2),(x+0,y-2),(x+1,y-2),(x+2,y-2)]

prettyPrint :: [Word8] -> [Point] -> String
prettyPrint bs ps =
  let
    s = show pp_scale
    h = show pp_halfscale
  in
    "%!\n\n" ++
    "/drawcircle { newpath\n" ++
    "moveto\n" ++
    h ++ " 0.0 rmoveto\n" ++
    h ++ " 0.0 360.0 arc\n" ++
    "closepath\n" ++
    "0.5 0.5 sethsbcolor\nfill } def\n\n" ++
    concatMap pp (zip (reverse ps) bs) ++ "\nshowpage\n"

pp ((x,y), v) = h ++ " " ++ x' ++ " " ++ y' ++ " " ++ x' ++ " " ++ y' ++ " drawcircle\n"
  where
    x' = show $ pp_cX + pp_scale * (fromIntegral x)
    y' = show $ pp_cY + pp_scale * (fromIntegral y)
    h  = show $ (fromIntegral v) / 255.0

pp_scale :: Fractional a => a
pp_scale = 3.0
pp_halfscale :: Fractional a => a
pp_halfscale = pp_scale / 2.0
pp_cX :: Fractional a => a
pp_cX = 612.0 / 2.0
pp_cY :: Fractional a => a
pp_cY = 792.0 / 2.0


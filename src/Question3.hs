import Data.List.Split      (splitWhen)
import Data.Maybe           (mapMaybe)
import Prelude hiding (Right, Left) 
import Data.Char (digitToInt)
import Data.Function ((&))
import Debug.Trace (traceShowId, trace)

main :: IO()
main = do 
    file <- readFile "../input/input3.txt"
    putStrLn $ (show . solve1 . map (wireVectors . splitWhen (== ',')) . lines) file

data  Vec  =  Up Int | Right Int | Down Int | Left Int  
type Coord = (Int,Int) 
type Line = (Coord,Coord)

vectorise :: String -> Maybe Vec
vectorise (dir:dist)
    | dir == 'U' = Just $ Up (read dist)   
    | dir == 'R' = Just $ Right (read dist)   
    | dir == 'D' = Just $ Down (read dist)   
    | dir == 'L' = Just $ Left (read dist)   
    | otherwise = Nothing
vectorise _ = Nothing

wireVectors :: [String] -> [Vec]
wireVectors dirs = mapMaybe vectorise dirs

applyVec :: Vec -> Coord -> Coord
applyVec v (x,y) = case v of 
    Up d -> (x,y+d)
    Right d -> (x+d,y)
    Down d -> (x,y-d)
    Left d -> (x-d,y)

-- begin solving part 1
solve1 :: [[Vec]] -> Maybe Int
solve1 (v1:v2:vs) = do 
    let points1 = getPoints v1 
        points2 = getPoints v2
        overlappingPoints = getAllOverlaps points1 points2
    if overlappingPoints == [] then Nothing
    else Just $ (minimum . map hamiltonianDist) overlappingPoints
solve1 _ = Nothing

-- Generates every single point of the wires
getPoints :: [Vec] -> [Coord]
getPoints vecs = points (0,0) vecs [] 
    where 
        points :: Coord -> [Vec] -> [Coord] -> [Coord]
        points p [] ps = ps 
        points p (v:vs) ps 
            | mag v == 0 = ps ++ (points p vs [])
            | otherwise = points (oneStepFromP p v) (reduced v:vs) (oneStepFromP p v : ps)
            
        mag :: Vec -> Int 
        mag v = case v of 
            Up a -> a
            Right a -> a
            Down a -> a
            Left a -> a

        reduced :: Vec -> Vec 
        reduced v = case v of
            Up a -> Up (a-1)
            Right a -> Right (a-1)
            Down a -> Down (a-1)
            Left a -> Left (a-1)

        oneStepFromP :: Coord -> Vec -> Coord
        oneStepFromP (x,y) v = case v of 
            Up a -> (x,y+1)
            Right a -> (x+1,y)
            Down a -> (x,y-1)
            Left a -> (x-1,y)
    
hamiltonianDist :: Coord -> Int 
hamiltonianDist (x,y) = (abs x) + (abs y) 

-- cartesian product... not great ...
getAllOverlaps :: [Coord] -> [Coord] -> [Coord]
getAllOverlaps xs ys = [x | x <- xs , y <- ys , x == y]
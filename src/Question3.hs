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

-- solve part a
solve1 :: [[Vec]] -> Maybe Int
solve1 (v1:v2:vs) = do 
    let lines1 = getLines v1 
        lines2 = getLines v2
        overlappingPoints = getAllOverlaps lines1 lines2
    if overlappingPoints == [] then Nothing
    else Just $ (minimum . map hamiltonianDist) overlappingPoints
solve1 _ = Nothing

-- TODO this is wrong, dont use init but just discount 0,0 position
getLines :: [Vec] -> [Line]
getLines vecs = init $ helper (0,0) vecs [] 
    where helper :: Coord -> [Vec] -> [Line] -> [Line] 
          helper p [] ls = ls
          helper p (v:vs) ls = helper newP vs ((p, newP) : ls)
            where newP = applyVec v p 

getAllOverlaps :: [Line] -> [Line] -> [Coord]
getAllOverlaps xs ys = do 
    [getClosestIntersection x y| x <- xs , y <- ys , x `intersects` y]
    
hamiltonianDist :: Coord -> Int 
hamiltonianDist (x,y) = (abs x) + (abs y) 

getClosestIntersection :: Line -> Line -> Coord
getClosestIntersection line1@((x1,y1),(x2,y2)) line2@((x3,y3),(x4,y4)) 
    | inCross line1 line2 = (getRepeated [x1,x2,x3,x4], getRepeated [y1,y2,y3,y4])
    | otherwise =  if horizontal line1 then (getRepeated [x1,x2,x3,x4], y1)
                   else (x1, getRepeated [y1,y2,y3,y4])
    where getRepeated xs = helper xs []
          helper (x:xs) seen = 
            if x `elem` seen then x 
            else helper xs (x:seen)
          
            -- either a cross or co-linear intersection formation
          inCross ((x1,y1),(x2,y2)) ((x3,y3),(x4,y4)) = not $ (all (==x1) [x1,x2,x3,x4]) || (all (==y1) [y1,y2,y3,y4])

intersects :: Line -> Line -> Bool 
intersects (x1,y1) (x2,y2) = line1Interects && line2Intersects
    where line1Interects = validIntersection (sideOn x1 (x2,y2)) (sideOn y1 (x2,y2))   
          line2Intersects = validIntersection (sideOn x2 (x1,y1)) (sideOn y2 (x1,y1))   

-- Logic and Data types for determining intersections
data Placement = LeftOrBelowLine | RightOrAboveLine | OnLine

validIntersection :: Placement -> Placement -> Bool
validIntersection LeftOrBelowLine LeftOrBelowLine = False
validIntersection RightOrAboveLine RightOrAboveLine = False
validIntersection _ _ = True

sideOn :: Coord -> Line -> Placement 
sideOn p (x,y) = if horizontal (x,y) then above (x,y) p 
                 else right (x,y) p 

horizontal ((_,y1), (_,y2)) = y1==y2
above ((_,y1), _) (_,y2) 
            | y2 > y1 = RightOrAboveLine
            | y2 == y1 = OnLine
            | y2 < y1 = LeftOrBelowLine

right ((x1,_),_) (x2,_) 
            | x2 > x1 = RightOrAboveLine
            | x2 == x1 = OnLine 
            | x2 < x1 = LeftOrBelowLine

-- solve part b
-- solve2 :: [[Vec]] -> Maybe Int
-- solve1 (v1:v2:vs) = do 
--     let points1 = getLines v1 
--         points2 = getLines v2
--         overlappingPoints = getAllOverlaps points1 points2
--     if overlappingPoints == [] then Nothing
--     else Just $ (minimum . map hamiltonianDist) overlappingPoints
-- solve1 _ = Nothing

-- type LineWithStep = (Line, Int)

-- use LineWithStep instead of line
-- As we generate lines we need to accumulate steps 
-- add logic to deduce the step at a given coord
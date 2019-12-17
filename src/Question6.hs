import Data.List.Split      (splitWhen)
import Data.Maybe           (mapMaybe, fromMaybe)
import Prelude hiding (Right, Left) 
import Data.Char (digitToInt)
import Data.Function ((&))
import Debug.Trace (traceShowId, trace)

import qualified Data.HashMap.Strict  as HM 
import qualified Data.HashSet as HS 

main :: IO()
main = do 
    file <- readFile "../input/input6.txt"
    putStrLn $ (show . solve2 . map (convertToOrbit . splitWhen (== ')')) . lines) file

data Orbit = Orbit String String 
convertToOrbit [planetA, planetB] = Orbit planetA planetB

solve1 :: [Orbit] -> Int 
solve1 orbits = fancyDepthFirstSearchCount orbitsRelation 
    where orbitsRelation = generateHM orbits HM.empty

fancyDepthFirstSearchCount :: HM.HashMap String [String] -> Int 
fancyDepthFirstSearchCount hm = cleverThing "COM" 0 
    where cleverThing :: String -> Int -> Int 
          cleverThing currentNode depth = depth + sum [cleverThing child (depth+1) | child <- childList]
            where childList = fromMaybe [] $ HM.lookup currentNode hm  

generateHM :: [Orbit] -> HM.HashMap String [String] -> HM.HashMap String [String]
generateHM [] hm = hm 
generateHM (Orbit a b : os ) hm = generateHM os updatedMap
    where 
        mChildren = HM.lookup a hm   
        updatedChildren = maybe [b] (b:) mChildren
        updatedMap =  HM.insert a updatedChildren hm


solve2 :: [Orbit] -> Int 
solve2 orbits = dfs connectedTo start end
    where connectedTo = generateHM (map flipOrbit orbits)  $ generateHM orbits HM.empty
          start = getOrbitOf "YOU" orbits
          end = getOrbitOf "SAN" orbits
          getOrbitOf :: String -> [Orbit] -> String
          getOrbitOf toFind ((Orbit a b):os)
            | b == toFind = a 
            | otherwise = getOrbitOf toFind os 
          flipOrbit (Orbit a b) = Orbit b a 


dfs :: HM.HashMap String [String] -> String -> String -> Int 
dfs hm start end = fromMaybe (-1) $ minimum <$> dfsHelper hm (HS.empty) start end 0 
        where dfsHelper :: HM.HashMap String [String] -> HS.HashSet String -> String -> String -> Int -> Maybe [Int] 
              dfsHelper hm visited curr destination distance = 
                if HS.member curr visited then Nothing 
                else if curr == destination then Just [distance]
                     else Just $ concat $ mapMaybe (\next -> dfsHelper hm (HS.insert curr visited) next destination (distance+1)) connectedNodes
                     where 
                        connectedNodes = fromMaybe [] $ HM.lookup curr hm   


-- type Queue = ([String], [String])
-- getFirst ([],xs) = head xs 
-- getFirst (xs,_) = head xs 
-- addList ys ([],xs) = (xs,ys)
-- addList ys (xs,zs) = (xs, zs++ys)

-- fancyBreadthFirstSearch :: HM.HashMap String [String] -> String -> String -> Int 
-- fancyBreadthFirstSearch hm start end = trace "BeginBFS" $ fromMaybe (-1) $ bfsHelper hm  ([start],[]) end 0 
--         where 
--             bfsHelper :: HM.HashMap String [String] -> HS.HashSet String -> Queue -> String  -> Int -> Maybe Int 
--             bfsHelper hm visited ([],[]) destination distance = Nothing 
--             bfsHelper hm visited queue destination distance = 
--                 if curr == destination then Just distance
--                 else if HS.member curr visited then bfsHelper hm (queue) destination (distance+1) 
--                 where 
--                     curr = getFirst queue
--                     children = fromMaybe [] $ HM.lookup curr hm 
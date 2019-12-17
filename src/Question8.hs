import Data.List.Split      (splitWhen, chunksOf)
import Data.List (minimumBy)
import Data.Maybe           (mapMaybe, fromMaybe)
import Prelude hiding (Right, Left) 
import Data.Char (digitToInt, intToDigit)
import Data.Function ((&))
import Debug.Trace (traceShowId, trace)
import Data.Tuple.Utils (fst3)

import qualified Data.HashMap.Strict  as HM 
import qualified Data.HashSet as HS 

main :: IO()
main = do 
    file <- readFile "../input/input8.txt"
    let image = chunksOf 25 $ map toFun $ (solve2 . map digitToInt) file
    mapM_ putStrLn image

toHumanReadable 1 = '#'
toHumanReadable 0 = ' '

solve1 :: [Int] -> Int 
solve1 [] = 0 
solve1 xs = oneCount*twoCount 
    where (zeroCount,oneCount,twoCount) = getLayers (25,6) xs (25*6+1,0,0) 
    

type CountOfVals = (Int,Int,Int)

customMinimum :: CountOfVals -> CountOfVals -> CountOfVals
customMinimum currMin x
    | fst3 x < fst3 currMin = x
    | otherwise = currMin 

getLayers :: (Int, Int) -> [Int] -> CountOfVals -> CountOfVals
getLayers (w,h) [] currMin = currMin
getLayers (w,h) pixels currMin = getLayers (w,h) remainingPixels minSoFar
    where minSoFar = customMinimum currMin (counter pixelsInLayer)
          pixelsInLayer = take (w*h) pixels
          remainingPixels = drop (w*h) pixels

counter :: [Int] -> CountOfVals
counter xs = (count 0, count 1, count 2)
    where count x = length $ filter (==x) xs 

solve2 :: [Int] -> [Int]
solve2 xs = getImage (25,6) xs 

getImage :: (Int, Int) -> [Int] -> [Int]
getImage (w,h) pixels = [getPixel i layers | i <- [0..((w*h)-1)]]
    where layers = getAllLayers (w,h) pixels
          getPixel :: Int -> [Layer] -> Int 
          getPixel _ [] = 0 
          getPixel i (topLayer:otherLayers) = case topLayer !! i of 
            0 -> 0 
            1 -> 1 
            2 -> getPixel i otherLayers

type Layer = [Int]
getAllLayers :: (Int, Int) -> [Int] -> [Layer]
getAllLayers _ [] = []
getAllLayers (w,h) pixels = thisLayer : otherLayers
    where thisLayer = take (w*h) pixels
          otherLayers = getAllLayers (w,h) $ drop (w*h) pixels
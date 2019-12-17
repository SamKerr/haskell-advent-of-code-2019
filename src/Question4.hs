import Data.Char (digitToInt)

main :: IO()
main = (putStrLn . show) $ solve1 382345 843167

-- solve 1 and 2 were extrememly similar here so I have not bothered to seperate things
solve1 :: Int -> Int -> Int 
solve1 lowerBound upperBount = length $ filter isValid [lowerBound..upperBount]

isValid :: Int -> Bool
isValid value = containsRepeat valueList && allAscending valueList 
    where 
      valueList = map digitToInt $ show value

      containsRepeat :: [Int] -> Bool 
      containsRepeat [] = False 
      containsRepeat [x] = False
      containsRepeat [x1,x2] = x1 == x2 
      containsRepeat (x1:x2:x3:xs) 
        | x1 == x2 && x2 /= x3 = True 
        | otherwise = containsRepeat $ dropWhile (==x1) (x1:x2:x3:xs)

      allAscending :: [Int] -> Bool 
      allAscending [] = True 
      allAscending [x] = True 
      allAscending (x1:x2:xs) 
        | x2 < x1 = False 
        | otherwise = allAscending (x2:xs)


-- =<< is f :: a -> b and m a gets m b
-- <$> is fmap 
main :: IO
main = do 
    file <- readFile "../input/input1.txt"
    putStrLn $ (show . recursiveTotalFuelFromMass . map read . lines) file 

fuelFromMass :: Int -> Int 
fuelFromMass mass = (mass `div` 3)-2

totalFuelFromMass :: [Int] -> Int
totalFuelFromMass = sum . map fuelFromMass

recursiveFuelFromMass :: Int -> Int 
recursiveFuelFromMass mass = helper mass 0 
    where 
        helper :: Int -> Int -> Int
        helper mass total = 
            if fuelFromMass mass <= 0 then total
            else helper (fuelFromMass mass) (total + fuelFromMass mass)

recursiveTotalFuelFromMass :: [Int] -> Int
recursiveTotalFuelFromMass = sum . map recursiveFuelFromMass


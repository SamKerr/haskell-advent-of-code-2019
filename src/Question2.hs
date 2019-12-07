import Data.List.Split      (splitWhen)
import Data.List            (find)
import Data.Maybe
import qualified Data.HashMap.Strict  as HM 

main :: IO()
main = do 
    file <- readFile "../input/input2.txt"
    putStrLn $ (show . solve2 . map read . splitWhen (== ',')) file 

pairIndexAndInstructions :: [Int] -> [(Int, Int)]
pairIndexAndInstructions vals = helper vals 0
    where
        helper :: [Int] -> Int -> [(Int, Int)]
        helper [] i = []
        helper (v:vs) i = (i,v) : helper vs (i+1)

decodeAndExecute :: Int -> Int -> Int -> Int -> HM.HashMap Int Int -> HM.HashMap Int Int
decodeAndExecute x1 x2 x3 x4 states = case x1 of
    1 -> maybe states (\v -> HM.insert x4 v states) (maybeSum valAtX2 valAtX3)  
    2 -> maybe states (\v -> HM.insert x4 v states) (maybeProd valAtX2 valAtX3)  
    where 
        valAtX2 = HM.lookup x2 states 
        valAtX3 = HM.lookup x3 states
        maybeSum ma mb = (+) <$> ma <*> mb
        maybeProd ma mb = (*) <$> ma <*> mb

execute :: [Int] -> HM.HashMap Int Int -> Maybe Int
execute (99 : xs) states = HM.lookup 0 states  
execute (op1:op2:op3:op4:instrs) states = do 
    let updatedStates = decodeAndExecute op1 op2 op3 op4 states
    execute instrs updatedStates
execute _ states = Nothing
      
solve1 :: [Int] -> Int
solve1 instructions = do
    let indexInstructionPairs = pairIndexAndInstructions instructions
        states = HM.fromList indexInstructionPairs 
    fromMaybe (-1) (execute instructions states)
    
solve2 :: [Int] -> Int 
solve2 instructions = maybe (-1)  (\(n,v)->100*n+v) (find isValidNounVerbPair nvPossibilities)
    where 
        indexInstructionPairs = pairIndexAndInstructions instructions
        initialStates = HM.fromList indexInstructionPairs 
        
        nvPossibilities = [(n,v) | n <-[0..99], v<-[0..99]]

        isValidNounVerbPair :: (Int,Int) -> Bool
        isValidNounVerbPair (n,v) = do 
            let newStates = updatedStates initialStates n v
                newInstr = updateInstr instructions n v
            (fromMaybe (-1) (execute newInstr newStates)) == 19690720  

        updateInstr :: [Int] -> Int -> Int -> [Int]
        updateInstr (in1:in2:in3:instrs) noun verb = in1:noun:verb:instrs 
        
        updatedStates :: HM.HashMap Int Int -> Int -> Int -> HM.HashMap Int Int 
        updatedStates hm noun verb = do 
            let newHM = HM.insert 1 noun initialStates 
            HM.insert 2 verb newHM
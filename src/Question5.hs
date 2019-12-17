import Data.List.Split      (splitWhen)
import Data.List            (find)
import Data.Maybe
import qualified Data.HashMap.Strict  as HM 
import Debug.Trace (traceShowId, trace)

main :: IO()
main = do 
    file <- readFile "../input/input5.txt"
    solution <- (solve1 . map read . splitWhen (== ',')) file 
    putStrLn $ show solution

pairIndexAndInstructions :: [Int] -> [(Int, Int)]
pairIndexAndInstructions vals = helper vals 0
    where
        helper :: [Int] -> Int -> [(Int, Int)]
        helper [] i = []
        helper (v:vs) i = (i,v) : helper vs (i+1)

boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0

type States = HM.HashMap Int Int  
data PartialFunction = RecieveArg (Arg -> ExecutionState) 
type Arg = (ExecutionState, Int)
data ExecutionState = ExecutionState 
  { state  :: States   
  , pc     :: Int 
  }   

setState :: HM.HashMap Int Int -> ExecutionState -> ExecutionState
setState hm ex = ex {state = hm}

increasePC :: Int -> ExecutionState -> ExecutionState
increasePC inc ex = ex {pc = (pc ex) + inc}

setPC :: Int -> ExecutionState -> ExecutionState
setPC newPC ex = ex {pc = newPC}

type Opcode = Int 
type Instruction = Int 
type Operand = (Int,Mode)
data Mode = Position | Immediate deriving Show    

getOperandValue :: Operand -> States -> Int 
getOperandValue (opVal,Immediate) _ = opVal
getOperandValue (opVal, Position) currStates = fromMaybe (trace "FAILED TO FIND VALUE" 0) $ HM.lookup opVal currStates  
getImmediate (opVal, _) = opVal
getPosition (opVal, _) currStates = fromMaybe (trace "FAILED TO FIND VALUE" 0) $ HM.lookup opVal currStates

-- Handles 
    -- maths +,*
    -- contitions < , ==
execute :: (Opcode, [Operand]) -> ExecutionState -> IO ExecutionState
execute (op1,[op2,op3,op4]) executionState = do 
    let currStates = state executionState   
        [val2,val3] = map (\operand -> getOperandValue operand currStates) [op2,op3]
        val4Imm = getImmediate op4 --op4 is always an immediate of an address
        setValueAtVal4 val = return $ setState (HM.insert val4Imm val currStates) executionState 
    case op1 of
        1 -> setValueAtVal4 (val2 + val3)   
        2 -> setValueAtVal4 (val2 * val3) 
        7 -> setValueAtVal4 (boolToInt $ val2 < val3)
        8 -> setValueAtVal4 (boolToInt $ val2 == val3)

-- Handles IO instructions
execute (op1,[op2]) executionState = do
    let val2Imm = getImmediate op2
        val2    = getOperandValue op2 $ state executionState 
    case op1 of 
        3 -> do 
            input <- getLine
            return $ setState (HM.insert val2Imm (read input) (state executionState)) executionState 
        4 -> do 
            (putStrLn . show) val2
            return executionState

-- handles jumps 
execute (op1,[op2,op3]) executionState = do 
    let val3 = getOperandValue op3 $ state executionState
        val2 = getOperandValue op2 $ state executionState
    case op1 of 
        5 -> if val2 /= 0 then return $ setPC val3 executionState
             else return executionState
        6 -> if val2 == 0 then return $ setPC val3 executionState
             else return executionState

getOpcodeAndModes :: Instruction -> (Opcode, [Mode])
getOpcodeAndModes instruction = (read opcode, parameterModes)
    where strInstr = show instruction
          (modes,opcode) = splitAt (length strInstr - 2) $ strInstr
          parameterModes = (map getMode $ reverse modes) ++ repeat Position
          getMode c  
            | c == '0' = Position
            | c == '1' = Immediate
            | otherwise = trace "Could not decode operand mode" Position

decode :: Instruction -> ExecutionState -> ((Opcode, [Operand]), Int)
decode i executionState = case getOpcodeAndModes i of 
    (1,modes) -> ((1, toOps 3 modes), 4) 
    (2,modes) -> ((2, toOps 3 modes), 4) 
    (3,modes) -> ((3, toOps 1 modes), 2) 
    (4,modes) -> ((4, toOps 1 modes), 2)
    (5,modes) -> ((5, toOps 2 modes), 3)
    (6,modes) -> ((6, toOps 2 modes), 3)
    (7,modes) -> ((7, toOps 3 modes), 4)
    (8,modes) -> ((8, toOps 3 modes), 4)
    _ -> trace "Operand not recognised" $ ((4, toOps 1 []), 2)
    where toOps operandCount modes = zip (getNextK operandCount) modes
          getNextK k = mapMaybe (\key -> HM.lookup key (state executionState)) [currPc+1..currPc+k]
          currPc = pc executionState

-- Halt if instruction is 99
decodeAndExecute :: Int -> ExecutionState -> IO ExecutionState
decodeAndExecute 99 executionState = return executionState
decodeAndExecute instruction executionState = do 
    let (ops,pcIncriment) = decode instruction executionState
    updatedStates <- execute ops executionState
    let jumpNotPerfomed = pc updatedStates == pc executionState
        updatedPCAndStates =  if jumpNotPerfomed then increasePC pcIncriment updatedStates
                              else updatedStates
    runUntilHalt updatedPCAndStates

fetch :: ExecutionState -> Int 
fetch executionState = fromMaybe (trace "PC did not point to instruction in state" 0) $ HM.lookup (pc executionState) (state executionState)

runUntilHalt :: ExecutionState -> IO ExecutionState
runUntilHalt executionState = decodeAndExecute (fetch executionState) executionState
    
solve1 :: [Int] -> IO ()
solve1 instructions = do 
    runUntilHalt initialEx 
    return ()
    where indexInstructionPairs = pairIndexAndInstructions instructions
          initialEx = ExecutionState {state = HM.fromList indexInstructionPairs, pc = 0} 
    
    
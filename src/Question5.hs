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

-- state of the program
-- The stack is unneccsary but thought it might be useful for later questions
type States = HM.HashMap Int Int  
data PartialFunction = RecieveArg (Arg -> ExecutionState) 
type Arg = (ExecutionState, Int)
data ExecutionState = ExecutionState 
  { state  :: States   
  , stack  :: [PartialFunction]  
  }   

setState :: HM.HashMap Int Int -> ExecutionState -> ExecutionState
setState hm ex = ex {state = hm}

pushStack :: PartialFunction -> ExecutionState -> ExecutionState
pushStack f ex = ex {stack = (f:(stack ex))}

popStack :: ExecutionState -> (PartialFunction,ExecutionState) 
popStack ex = (top,newEx)
    where top = (head . stack) ex 
          newEx = ex {stack = (tail . stack) ex}

type Opcode = Int 
type Instruction = Int 
type Operand = (Int,Mode)
data Mode = Position | Immediate deriving Show    

getOperandValue :: Operand -> States -> Int 
getOperandValue (opVal,Immediate) _ = opVal
getOperandValue (opVal, Position) currStates = fromMaybe (trace "FAILED TO FIND VALUE" 0) $ HM.lookup opVal currStates  
getImmediate (opVal, _) = opVal
getPosition (opVal, _) currStates = fromMaybe (trace "FAILED TO FIND VALUE" 0) $ HM.lookup opVal currStates

-- Handles + and * instructions
execute :: (Opcode, [Operand]) -> ExecutionState -> IO ExecutionState
execute (op1,[op2,op3,op4]) executionState = do 
    let currStates = state executionState   
        [val2,val3] = map (\operand -> getOperandValue operand currStates) [op2,op3]
        val4Imm = getImmediate op4 --op4 is always an immediate of an address
    case op1 of
        1 -> return $ setState (HM.insert val4Imm (val2 + val3) currStates) executionState  
        2 -> return $ setState (HM.insert val4Imm (val2 * val3) currStates) executionState

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

-- handles conditions 


getOpcodeAndModes :: Instruction -> (Opcode, [Mode])
getOpcodeAndModes instruction = (read opcode, parameterModes)
    where strInstr = show instruction
          (modes,opcode) = splitAt (length strInstr - 2) $ strInstr
          parameterModes = (map getMode $ reverse modes) ++ repeat Position
          getMode c  
            | c == '0' = Position
            | c == '1' = Immediate
            | otherwise = trace "Could not decode operand mode" Position

decode :: Instruction -> Int -> ExecutionState -> ((Opcode, [Operand]), Int)
decode i pc executionState = case getOpcodeAndModes i of 
    (1,modes) -> ((1, toOps 3 modes), pc+4)
    (2,modes) -> ((2, toOps 3 modes), pc+4)
    (3,modes) -> ((3, toOps 1 modes), pc+2)
    (4,modes) -> ((4, toOps 1 modes), pc+2)
    _ -> trace "Operand not recognised" $ ((4, toOps 1 []), pc+2)
    where toOps operandCount modes = zip (getNextK operandCount) modes
          getNextK k = mapMaybe (\key -> HM.lookup key (state executionState)) [pc+1..pc+k]

run :: Int -> Int -> ExecutionState -> IO ExecutionState
run 99 _ executionState = return executionState
run instruction pc executionState = do 
    let (ops,newPc) = decode instruction pc executionState
    updatedStates <- execute ops executionState
    runFrom newPc updatedStates

runFrom :: Int -> ExecutionState -> IO ExecutionState
runFrom pc executionState = do 
    let instr = fromMaybe (trace "PC did not point to instruction in state" 0) $ HM.lookup pc (state executionState)
    run instr pc executionState

solve1 :: [Int] -> IO ()
solve1 instructions = do 
    runFrom 0 initialEx 
    return ()
    where indexInstructionPairs = pairIndexAndInstructions instructions
          initialEx = ExecutionState {state = HM.fromList indexInstructionPairs, stack =[]} 
    
    
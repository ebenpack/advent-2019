{-# LANGUAGE RecordWildCards #-}

module IntCode where

import Util

import Data.List
import Data.Vector (Vector, fromList, (!), (//))
import qualified Data.Vector as V ((++))
import Control.Parallel.Strategies

type IntCode = Vector Integer

data OpCode
    = Add
    | Mul
    | Halt
    | Input
    | Output
    | JumpIfTrue
    | JumpIfFalse
    | LessThan
    | AdjustRelativeBase
    | Equals
    deriving (Show, Eq)

data ParameterMode
    = PositionMode
    | ImmediateMode
    | RelativeMode
    deriving (Show, Eq)

data ProgramStatus
    = Running
    | Suspended
    | Halted
    deriving (Show, Ord, Eq)

data Program = Program
    { intcodes_ :: IntCode
    , instructionPointer_ :: Int
    , status_ :: ProgramStatus
    , inputs_ :: [Integer]
    , outputs_ :: [Integer]
    , relativeBase_ :: Int
    } deriving (Eq, Ord, Show)

intToOpCode :: Integer -> (OpCode, [ParameterMode])
intToOpCode n =
    let ns = intToIntList n
        opcode = intToOpCode' $ intListToInt $ takeLast 2 ns
        params = intToParam <$> leftPad (paramsForOpCode opcode) (dropLast 2 ns)
    in (opcode, reverse params)
    where
    paramsForOpCode Add = 3
    paramsForOpCode Mul = 3
    paramsForOpCode Halt = 0
    paramsForOpCode Input = 1
    paramsForOpCode Output = 1
    paramsForOpCode JumpIfTrue = 2
    paramsForOpCode JumpIfFalse = 2
    paramsForOpCode LessThan = 3
    paramsForOpCode AdjustRelativeBase = 1
    paramsForOpCode Equals = 3
    intToParam 0 = PositionMode
    intToParam 1 = ImmediateMode
    intToParam 2 = RelativeMode
    intToOpCode' 1 = Add
    intToOpCode' 2 = Mul
    intToOpCode' 3 = Input
    intToOpCode' 4 = Output
    intToOpCode' 5 = JumpIfTrue
    intToOpCode' 6 = JumpIfFalse
    intToOpCode' 7 = LessThan
    intToOpCode' 8 = Equals
    intToOpCode' 9 = AdjustRelativeBase
    intToOpCode' 99 = Halt

mkProgram :: [Integer] -> Program
mkProgram xs = Program (fromList xs) 0 Running [] [] 0

readValue :: ParameterMode -> Program -> Int -> Integer
readValue param Program{..} ix =
    case param of
        PositionMode -> getFrom (fromIntegral $ intcodes_ ! ix)
        RelativeMode -> getFrom (fromIntegral (intcodes_ ! ix) + relativeBase_)
        ImmediateMode -> getFrom ix
    where
    getFrom ix =
        if ix >= length intcodes_
        then 0
        else intcodes_ ! ix

writeValue :: ParameterMode -> Program -> Int -> Integer -> Program
writeValue param p@Program{..} ix value =
    let offset = if param == RelativeMode then relativeBase_ else 0
        address = fromIntegral (intcodes_ ! ix ) + offset
        newIntcodes = setIn (fromIntegral address) value
    in p { intcodes_ = newIntcodes }
    where
    setIn :: Int -> Integer -> Vector Integer
    setIn ix value =
        let newIntcode = if ix >= length intcodes_
                         then intcodes_ V.++ fromList [0 | _ <- [length intcodes_..ix]]
                         else intcodes_
        in newIntcode // [(ix, value)]

runOp :: Program -> OpCode -> [ParameterMode] -> Program
runOp p@Program{..} op params = go op
    where
    go Add = runBinaryOp (+)
    go Mul = runBinaryOp (*)
    go Input =
        case inputs_ of
            [] -> p { status_ = Suspended } -- Block waiting on input
            (x:xs) -> runIO (\ix ->
                let newProg = writeValue (head params) p (instructionPointer_ + 1) x
                in newProg { inputs_ = xs })
    go Output = runIO (\val -> p { outputs_ = val:outputs_ })
    go JumpIfTrue = runJump (/= 0)
    go JumpIfFalse = runJump (== 0)
    go LessThan = runComp (<)
    go Equals = runComp (==)
    go AdjustRelativeBase =
        let operand1 = readValue (head params) p (instructionPointer_ + 1)
            newProg = p { relativeBase_ = fromIntegral operand1 + relativeBase_ }
        in newProg { instructionPointer_ = instructionPointer_ + 2 }
    go Halt = p { status_ = Halted }
    runComp op =
        let operand1 = readValue (head params) p (instructionPointer_ + 1)
            operand2 = readValue (params !! 1) p (instructionPointer_ + 2)
            result = op operand1 operand2
            value = if result then 1 else 0
            newProg = writeValue (params !! 2) p (instructionPointer_ + 3) value
        in newProg { instructionPointer_ = instructionPointer_ + 4 }
    runJump op =
        let operand1 = readValue (head params) p (instructionPointer_ + 1)
            operand2 = readValue (params !! 1) p (instructionPointer_ + 2)
            result = op operand1
        in
            if result
            then p { instructionPointer_ = fromIntegral operand2 }
            else p { instructionPointer_ = instructionPointer_ + 3 }
    runIO op =
        let operand1 = readValue (head params) p (instructionPointer_ + 1)
            newProg = op operand1
        in newProg { instructionPointer_ = instructionPointer_ + 2 }
    runBinaryOp op =
        let operand1 = readValue (head params) p (instructionPointer_ + 1)
            operand2 = readValue (params !! 1) p (instructionPointer_ + 2)
            newProg = writeValue (params !! 2) p (instructionPointer_ + 3) (op operand1 operand2)
        in newProg { instructionPointer_ = instructionPointer_ + 4 }

runStep :: Program -> Program
runStep p@Program{..} =
    let (opCode, params) = intToOpCode (intcodes_ ! instructionPointer_)
    in runOp p opCode params

runProgram :: Program -> Program
runProgram p@Program { status_ = status } =
    if status /= Running
    then error "not running, you dummy"
    else go p
    where
    go p =
        case runStep p of
            p@Program { status_ = Running } -> go p
            p -> p

runProgramWithAmplification :: Program -> [Integer] -> [Integer]
runProgramWithAmplification prog [a, b, c, d, e] =
    go
        [ prog { inputs_ = [a, 0] }
        , prog { inputs_ = [b] }
        , prog { inputs_ = [c] }
        , prog { inputs_ = [d] }
        , prog { inputs_ = [e] }
        ] []
    where
    go ls thrusterOutput =
        let [a, b, c, d, e] = map runProgram ls `using` parList rseq
        in
            if status_ e == Halted
            then outputs_ e ++ thrusterOutput
            else
                go
                    [ a {inputs_ = outputs_ e, outputs_ = []}
                    , b {inputs_ = outputs_ a, outputs_ = []}
                    , c {inputs_ = outputs_ b, outputs_ = []}
                    , d {inputs_ = outputs_ c, outputs_ = []}
                    , e {inputs_ = outputs_ d, outputs_ = []}
                    ] (outputs_ e ++ thrusterOutput)

findGreatest testprog =
    maximum $ map (head . runProgramWithAmplification testprog) (permutations [0,1,2,3,4])

findGreatestWithFeeback testprog =
    maximum $ map (head . runProgramWithAmplification testprog) (permutations [5,6,7,8,9])

testp = mkProgram [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]

testp2 = mkProgram [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10]


a = runProgramWithAmplification testp [9,8,7,6,5] -- 139629729
b = runProgramWithAmplification testp2 [9,7,8,5,6] -- 18216

c = mkProgram [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99] -- takes no input and produces a copy of itself as output.
d = mkProgram [1102,34915192,34915192,7,4,7,99,0] --  should output a 16-digit number.
e = mkProgram [104,1125899906842624,99] -- should output the large number in the middle.


boostprog = mkProgram [1102,34463338,34463338,63,1007,63,34463338,63,1005,63,53,1101,3,0,1000,109,988,209,12,9,1000,209,6,209,3,203,0,1008,1000,1,63,1005,63,65,1008,1000,2,63,1005,63,904,1008,1000,0,63,1005,63,58,4,25,104,0,99,4,0,104,0,99,4,17,104,0,99,0,0,1102,1,550,1027,1101,0,0,1020,1101,30,0,1004,1101,0,22,1014,1102,1,36,1009,1101,37,0,1007,1102,25,1,1010,1102,1,33,1012,1102,282,1,1029,1102,1,488,1025,1101,0,31,1019,1101,0,21,1008,1101,0,35,1015,1101,664,0,1023,1102,26,1,1001,1101,28,0,1016,1102,29,1,1005,1102,1,24,1002,1101,20,0,1018,1101,27,0,1013,1101,38,0,1017,1102,1,1,1021,1102,1,557,1026,1102,1,39,1000,1101,23,0,1006,1101,493,0,1024,1102,1,291,1028,1101,671,0,1022,1101,0,34,1003,1101,0,32,1011,109,10,21108,40,40,8,1005,1018,199,4,187,1105,1,203,1001,64,1,64,1002,64,2,64,109,-14,2108,30,8,63,1005,63,225,4,209,1001,64,1,64,1105,1,225,1002,64,2,64,109,3,2102,1,4,63,1008,63,34,63,1005,63,251,4,231,1001,64,1,64,1106,0,251,1002,64,2,64,109,12,2107,22,-5,63,1005,63,269,4,257,1105,1,273,1001,64,1,64,1002,64,2,64,109,20,2106,0,-3,4,279,1001,64,1,64,1106,0,291,1002,64,2,64,109,-16,21108,41,40,-3,1005,1012,311,1001,64,1,64,1105,1,313,4,297,1002,64,2,64,109,-13,2101,0,2,63,1008,63,30,63,1005,63,335,4,319,1105,1,339,1001,64,1,64,1002,64,2,64,109,-3,2102,1,4,63,1008,63,35,63,1005,63,359,1106,0,365,4,345,1001,64,1,64,1002,64,2,64,109,15,1205,6,377,1105,1,383,4,371,1001,64,1,64,1002,64,2,64,109,5,21102,42,1,-2,1008,1017,39,63,1005,63,403,1106,0,409,4,389,1001,64,1,64,1002,64,2,64,109,-17,21107,43,44,10,1005,1012,431,4,415,1001,64,1,64,1106,0,431,1002,64,2,64,109,14,21107,44,43,-4,1005,1012,451,1001,64,1,64,1106,0,453,4,437,1002,64,2,64,109,1,21102,45,1,-3,1008,1014,45,63,1005,63,479,4,459,1001,64,1,64,1105,1,479,1002,64,2,64,109,7,2105,1,0,4,485,1106,0,497,1001,64,1,64,1002,64,2,64,109,5,1206,-8,513,1001,64,1,64,1106,0,515,4,503,1002,64,2,64,109,-33,2101,0,7,63,1008,63,32,63,1005,63,535,1106,0,541,4,521,1001,64,1,64,1002,64,2,64,109,23,2106,0,8,1001,64,1,64,1106,0,559,4,547,1002,64,2,64,109,-1,21101,46,0,-5,1008,1013,46,63,1005,63,585,4,565,1001,64,1,64,1105,1,585,1002,64,2,64,109,-4,21101,47,0,2,1008,1016,44,63,1005,63,605,1105,1,611,4,591,1001,64,1,64,1002,64,2,64,109,-18,1207,4,38,63,1005,63,627,1106,0,633,4,617,1001,64,1,64,1002,64,2,64,109,5,2107,22,7,63,1005,63,649,1106,0,655,4,639,1001,64,1,64,1002,64,2,64,109,12,2105,1,10,1001,64,1,64,1106,0,673,4,661,1002,64,2,64,109,-10,1208,6,33,63,1005,63,693,1001,64,1,64,1106,0,695,4,679,1002,64,2,64,109,-7,2108,35,7,63,1005,63,715,1001,64,1,64,1106,0,717,4,701,1002,64,2,64,109,6,1208,5,37,63,1005,63,735,4,723,1106,0,739,1001,64,1,64,1002,64,2,64,109,-4,1202,5,1,63,1008,63,34,63,1005,63,765,4,745,1001,64,1,64,1105,1,765,1002,64,2,64,109,29,1206,-7,783,4,771,1001,64,1,64,1105,1,783,1002,64,2,64,109,-28,1201,6,0,63,1008,63,29,63,1005,63,809,4,789,1001,64,1,64,1106,0,809,1002,64,2,64,109,5,1202,2,1,63,1008,63,20,63,1005,63,829,1106,0,835,4,815,1001,64,1,64,1002,64,2,64,109,-1,1201,6,0,63,1008,63,35,63,1005,63,859,1001,64,1,64,1105,1,861,4,841,1002,64,2,64,109,2,1207,-3,25,63,1005,63,879,4,867,1105,1,883,1001,64,1,64,1002,64,2,64,109,13,1205,3,901,4,889,1001,64,1,64,1106,0,901,4,64,99,21101,0,27,1,21101,915,0,0,1106,0,922,21201,1,22987,1,204,1,99,109,3,1207,-2,3,63,1005,63,964,21201,-2,-1,1,21101,0,942,0,1106,0,922,22101,0,1,-1,21201,-2,-3,1,21101,0,957,0,1106,0,922,22201,1,-1,-2,1105,1,968,21202,-2,1,-2,109,-3,2105,1,0]


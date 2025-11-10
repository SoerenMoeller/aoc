import Data.List.Split (splitOn)
import Data.Bits

inputFile :: String
inputFile = "day-17-input.txt"

inputFileExample :: String
inputFileExample = "day-17-input-example.txt"

processInput :: String -> ((Int, Int, Int), [Int])
processInput input = ((a, b, c), program)
  where
    (regLines, e:programLine) = break null . lines $ input
    [a, b, c] = map (read . drop 12) regLines
    program   = map read . splitOn "," . drop 9 . head $ programLine

comboOperand :: (Int, Int, Int) -> Int -> Int
comboOperand (regA, _,    _   ) 4 = regA
comboOperand (_,    regB, _   ) 5 = regB
comboOperand (_,    _,    regC) 6 = regC
comboOperand (_,    _,    _   ) 7 = error "Should not happen"
comboOperand (_,    _,    _   ) o = o

regOp :: (Int, Int, Int) -> Int -> Int -> Int -> (Int, Int, Int)
regOp (regA, regB, regC) o co op = case op of
    0 -> (uReg, regB,            regC)
    6 -> (regA, uReg,            regC)
    7 -> (regA, regB,            uReg)
    1 -> (regA, regB `xor` o,    regC)
    2 -> (regA, co `mod` 8,      regC)
    4 -> (regA, regB `xor` regC, regC)
    _ -> (regA, regB,            regC) 
  where
    uReg = regA `shiftR` co

fst3 :: (a, b, c) -> a
fst3 (a, b, c) = a

solveA :: (Int, Int, Int) -> Int -> [Int] -> [Int]
solveA regs idx istr
    | idx >= length istr = []
    | otherwise          = output ++ solveA regs' idx' istr
  where
    (op, o) = (istr !! idx, istr !! (idx + 1))
    co      = comboOperand regs o
    regs'   = regOp regs o co op
    idx'    = if op == 3 && fst3 regs /= 0 then o else idx + 2
    output  = [co `mod` 8 | op == 5]

mainA :: IO ()
mainA = do
    input <- readFile inputFile
    let (regs, instr) = processInput input
    print . solveA regs 0 $ instr

discoverInput :: (Int -> [Int]) -> [Int] -> Int -> [Int]
discoverInput _        []      prev = [prev] 
discoverInput runWithA program prev = concatMap explore [0..7]  
  where
    explore :: Int -> [Int]
    explore i 
        | head output /= last program = []
        | otherwise = discoverInput runWithA (init program) newPrev
          where
            newPrev = 8 * prev + i
            output  = runWithA newPrev

mainB :: IO ()
mainB = do
    input <- readFile inputFile
    let ((_, rb, rc), instr) = processInput input
    let runWithA a = solveA (a, rb, rc) 0 instr
    print . minimum . discoverInput runWithA instr $ 0

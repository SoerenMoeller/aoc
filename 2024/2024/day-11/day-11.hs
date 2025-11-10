import Data.MemoTrie (memo, memo2)
import Data.List (foldl')

inputFile :: String
inputFile = "day-11-input.txt"

inputFileExample :: String
inputFileExample = "day-11-input-example.txt"

processInput :: String -> [Int]
processInput = map read . words

numDigits :: Int -> Int
numDigits n 
    | n < 0     = error "Expected natural numbers only."
    | n == 0    = 1                 
    | otherwise = floor (logBase 10 (fromIntegral n)) + 1

splitNum :: Int -> [Int]
splitNum x = [x `div` splitValue, x `mod` splitValue]
  where splitValue = (10 ^) . (`div` 2) . numDigits $ x

applyRule :: Int -> [Int]
applyRule 0 = [1]
applyRule x 
    | (even . numDigits) x = splitNum x
    | otherwise            = [x * 2024]

applyRuleMemo :: Int -> [Int]
applyRuleMemo = memo applyRule

solveA :: Int -> [Int] -> Int
solveA blinks = length . (!! blinks) . iterate (concatMap applyRuleMemo)

mainA :: IO ()
mainA = do
    input <- readFile inputFile
    processed <- return . processInput $ input
    print . solveA 25 $ processed

solveB :: Int -> Int -> Int
solveB = memo2 solveB'
  where
    solveB' :: Int -> Int -> Int
    solveB' stone 0      = 1
    solveB' 0     blinks = solveB 1 (blinks - 1)
    solveB' stone blinks 
        | (odd . numDigits) stone = solveB (stone * 2024) (blinks - 1)
        | otherwise               = sum . map (`solveB` (blinks - 1)) $ splitNum stone

mainB :: IO ()
mainB = do
    input <- readFile inputFile
    processed <- return . processInput $ input
    print . sum . map (`solveB` 75) $ processed

--main :: IO ()
--main =
-- do input <- [format|2024 11 %u& %n|]
--    print (solve 25 input)
--    print (solve 75 input)
--
--solve :: Int -> [Int] -> Int
--solve n input = sum (times n blinks (IntMap.fromListWith (+) [(i, 1) | i <- input]))
--
--blinks :: IntMap Int -> IntMap Int
--blinks stones = IntMap.fromListWith (+) [(stone', n) | (stone, n) <- IntMap.assocs stones, stone' <- blink stone]
--
--blink :: Int -> [Int]
--blink 0 = [1]         -- 0 -> 1
--blink n               -- split in half if even length
--  | (w, 0) <- length (show n) `quotRem` 2
--  , (l, r) <- n `quotRem` (10 ^ w)
--  = [l, r]
--blink n = [n * 2024]  -- otherwise multiply by 2024

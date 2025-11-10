import Control.Applicative (liftA2)
import Control.Monad (replicateM)

inputFile :: String
inputFile = "day-7-input.txt"

inputFileExample :: String
inputFileExample = "day-7-input-example.txt"

processInput :: String -> [(Int, [Int])]
processInput = map (liftA2 (,) (read . init . head) (map read . tail) . words) . lines 

generateOperations :: Num a => [a] -> [a -> a -> a] -> [[a -> a -> a]]
generateOperations xs = replicateM (length xs - 1)

applyOperations :: Num a => [a] -> [a -> a -> a] -> a
applyOperations (x:xs) ops = foldl (\acc (op, y) -> op acc y) x $ zip ops xs

canAddUp :: (Num a, Eq a) => [a -> a -> a] -> (a, [a]) -> Bool
canAddUp ops (x, xs) = any ((x ==) . applyOperations xs) $ generateOperations xs ops

solveA :: [(Int, [Int])] -> Int
solveA = sum . map fst . filter (canAddUp [(*), (+)])

mainA :: IO ()
mainA = do
    input <- readFile inputFile
    processed <- return . processInput $ input
    print . solveA $ processed

solveB :: [(Int, [Int])] -> Int
solveB = sum . map fst . filter (canAddUp [(*), (+), concatOp])
    where 
        concatOp :: Int -> Int -> Int
        concatOp x y = x * 10 ^ numDigits y + y

        numDigits :: Int -> Int
        numDigits 0 = 1  
        numDigits n = floor (logBase 10 (fromIntegral (abs n))) + 1

mainB :: IO ()
mainB = do
    input <- readFile inputFile
    processed <- return . processInput $ input
    print . solveB $ processed


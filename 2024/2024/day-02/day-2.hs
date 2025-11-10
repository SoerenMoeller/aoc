import Control.Applicative (liftA2)

inputFile :: String
inputFile = "day-2-input.txt"

processInput :: String -> [[Int]]
processInput = map (map read . words) . lines

isSafe :: [Int] -> Bool
isSafe xs = isMonotonic xs && allAdjacentSatisfy isAbsDistanceInRange xs

isMonotonic :: [Int] -> Bool
isMonotonic xs = any (`allAdjacentSatisfy` xs) [uncurry (<=), uncurry (>=)]

allAdjacentSatisfy :: ((Int, Int) -> Bool) -> [Int] -> Bool
allAdjacentSatisfy fn = all fn . liftA2 zip tail id

isAbsDistanceInRange :: (Int, Int) -> Bool
isAbsDistanceInRange (a, b) = liftA2 (&&) (>= 1) (<= 3) . abs $ (a - b)

solveA :: [[Int]] -> Int
solveA = length . filter isSafe

mainA :: IO ()
mainA = do
    input <- readFile inputFile
    processed <- return . processInput $ input
    print . solveA $ processed

removeOneOrZero :: Eq a => [a] -> [[a]]
removeOneOrZero xs = xs : [removeIndex i xs | i <- [0 .. length xs - 1]]

removeIndex :: Int -> [a] -> [a]
removeIndex i xs = take i xs ++ drop (i + 1) xs

solveB :: [[Int]] -> Int
solveB = length . filter (any isSafe . removeOneOrZero)

mainB :: IO ()
mainB = do 
    input <- readFile inputFile
    processed <- return . processInput $ input
    print . solveB $ processed


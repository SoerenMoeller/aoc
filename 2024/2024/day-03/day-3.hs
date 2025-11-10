import Text.Regex.TDFA ((=~))
import Data.List (isPrefixOf)

inputFile :: String
inputFile = "day-3-input.txt"

extractMultTuples :: String -> String -> [(Int, Int)]
extractMultTuples input pattern = map (\[a, b, c] -> (read b, read c)) result
    where 
        result :: [[String]]
        result = input =~ pattern

regexPattern :: String
regexPattern = "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)"

solveA :: [(Int, Int)] -> Int
solveA = sum . map (uncurry (*)) 

mainA :: IO ()
mainA = do
    input <- readFile inputFile
    processed <- return . extractMultTuples input $ regexPattern
    print . solveA $ processed

extractInput :: String -> [[String]]
extractInput input = input =~ pattern
    where 
        pattern :: String
        pattern = "do\\(\\)|don't\\(\\)|mul\\(([0-9]{1,3}),([0-9]{1,3})\\)"

transformMul :: [[String]] -> [[String]]
transformMul = map processEntry
    where
        processEntry :: [String] -> [String]
        processEntry [text, val1, val2]
            | "mul" `isPrefixOf` text = ["mul", val1, val2]
            | otherwise = [text, val1, val2]

solveB :: [[String]] -> Int
solveB input = solveRecur input True 0
    where
        -- save current sum and if mul is enabled
        solveRecur :: [[String]] -> Bool -> Int -> Int
        solveRecur [["mul", x, y]] True val = read x * read y + val
        solveRecur [[_, _, _]] _ val = val
        solveRecur (["mul", x, y]:xs) True val = solveRecur xs True $ read x * read y + val
        solveRecur (["mul", _, _]:xs) False val = solveRecur xs False val
        solveRecur (["do()", _, _]:xs) _ val = solveRecur xs True val
        solveRecur (["don't()", _, _]:xs) _ val = solveRecur xs False val
        solveRecur _ _ val = val
        
mainB :: IO ()
mainB = do
    input <- readFile inputFile
    processed <- return . transformMul . extractInput $ input
    print . solveB $ processed

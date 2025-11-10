import System.CPUTime
import Text.Printf
import Data.List.Split (splitOn)
import Data.List (transpose)

processInput :: String -> ([[Int]], [[Int]], Int)
processInput input = (
    map (map (pred . count '#') . transpose) . filter (all (== '#') . last) $ mxs,
    map (map (pred . count '#') . transpose) . filter (all (== '#') . head) $ mxs,
    length . head $ mxs
  )
  where mxs = splitOn [""] . lines $ input

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

allCombinations :: [a] -> [a] -> [(a, a)]
allCombinations xs ys = (,) <$> xs <*> ys

solveA :: [[Int]] -> [[Int]] -> Int -> Int
solveA keys locks height = 
    length
    . filter (all (<= height - 2))
    . map (uncurry (zipWith (+)))
    $ allCombinations keys locks

main :: IO ()
main = do
    let inputData        = "day-25-input.txt"
    let inputDataExample = "day-25-input-example.txt"
    
    (keys, locks, height) <- processInput <$> readFile inputData
    start <- getCPUTime
    putStrLn . (++) "Problem 1:  " . show . solveA keys locks $ height
    end <- getCPUTime
    printf "Time:       %.3fs\n\n" $ toSeconds start end
  where
    toSeconds :: Integer -> Integer -> Double
    toSeconds start end = fromIntegral (end - start) / (10^12)

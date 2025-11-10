import System.CPUTime
import Text.Printf

processInput :: String -> String
processInput = id

solveA :: String -> String
solveA = id

solveB :: String -> String
solveB = id

main :: IO ()
main = do
    let inputPath        = "input.txt"
    let inputPathExample = "input-example.txt"
    
    startTotal <- getCPUTime
    input <- processInput <$> readFile inputPath
    start <- getCPUTime
    putStrLn . (++) "Problem 1:  " . show . solveA $ input
    end <- getCPUTime
    printf "Time:       %.3fs\n\n" $ toSeconds start end

    start <- getCPUTime
    putStrLn . (++) "Problem 2:  " . show . solveB $ input
    end <- getCPUTime
    printf "Time:       %.3fs\n\n" $ toSeconds start end

    endTotal <- getCPUTime
    printf "Total Time: %.3fs\n" $ toSeconds startTotal endTotal 
  where
    toSeconds :: Integer -> Integer -> Double
    toSeconds start end = fromIntegral (end - start) / (10^12)

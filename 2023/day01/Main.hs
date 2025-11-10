import System.CPUTime
import Text.Printf
import Data.Char (isDigit, digitToInt)
import Data.List (isPrefixOf)

processInput :: (String -> String) -> String -> [[Int]]
processInput transform = map (map digitToInt . filter isDigit . transform) . lines

processInput' :: (String -> String) -> String -> [String]
processInput' transform = map transform . lines

solve :: [[Int]] -> Int
solve = sum . map ((+) <$> (* 10) . head <*> last)

wordToDigit :: String -> String
wordToDigit "one" = "1"
wordToDigit "two" = "2"
wordToDigit "three" = "3"
wordToDigit "four" = "4"
wordToDigit "five" = "5"
wordToDigit "six" = "6"
wordToDigit "seven" = "7"
wordToDigit "eight" = "8"
wordToDigit "nine" = "9"

digitWords :: [String]
digitWords = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

castDigits :: String -> String
castDigits "" = ""
castDigits (x:xs) = case startingDigits of 
    [] -> x : castDigits xs
    (y:_) -> y ++ castDigits xs
  where
    startingDigits = map wordToDigit . filter (`isPrefixOf` (x:xs)) $ digitWords

main :: IO ()
main = do
    let inputPath = "day01/input.txt"
    let inputPathExample = "day01/input-example.txt"

    startTotal <- getCPUTime
    inputA <- processInput id <$> readFile inputPath
    start <- getCPUTime
    putStrLn . (++) "Problem 1:  " . show . solve $ inputA
    end <- getCPUTime
    printf "Time:       %.3fs\n\n" $ toSeconds start end

    inputB <- processInput castDigits <$> readFile inputPath
    start <- getCPUTime
    putStrLn . (++) "Problem 2:  " . show . solve $ inputB
    end <- getCPUTime
    printf "Time:       %.3fs\n\n" $ toSeconds start end

    endTotal <- getCPUTime
    printf "Total Time: %.3fs\n" $ toSeconds startTotal endTotal
  where
    toSeconds :: Integer -> Integer -> Double
    toSeconds start end = fromIntegral (end - start) / (10^12)

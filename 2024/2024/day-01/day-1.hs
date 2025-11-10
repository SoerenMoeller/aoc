import Data.List (sort)
import Control.Applicative (liftA2)

inputFile :: String
inputFile = "day-1-input.txt"

processLines :: String -> ([Integer], [Integer])
processLines input = (map (read . head) linesOfWords, map (read . last) linesOfWords)
    where linesOfWords = map words $ lines input

sortLists :: Ord a => ([a], [a]) -> ([a], [a])
sortLists (xs, ys) = (sort xs, sort ys)

transformToTuples :: ([a], [a]) -> [(a, a)]
transformToTuples (xs, ys) = zip xs ys

addDiffs :: Num a => [(a, a)] -> a
addDiffs = sum . map (liftA2 ((abs .) . (-)) fst snd)

mainA :: IO ()
mainA = do
    input <- readFile inputFile
    let wordLists = sortLists . processLines $ input
    let tuplesList = transformToTuples wordLists
    let result = addDiffs tuplesList 
    print result

countContained :: Eq a => a -> [a] -> Integer
countContained x = toInteger . length . filter (x ==)

calcScore :: ([Integer], [Integer]) -> Integer
calcScore (xs, ys) = sum . map (liftA2 (*) (`countContained` ys) id) $ xs

mainB :: IO ()
mainB = do
    input <- readFile inputFile
    let wordLists = processLines input
    let result = calcScore wordLists
    print result
    

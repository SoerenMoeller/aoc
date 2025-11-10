import Data.List (sortBy, sort)
import Data.List.Split (splitOn)
import Data.Bifunctor (bimap)
import Control.Applicative (liftA2)

inputFile :: String
inputFile = "day-5-input.txt"

processInput :: String -> ([(Int, Int)], [[Int]])
processInput input = bimap (map parseOrderLine) (map parseUpdateLine) splitInput
    where 
        inputLines :: [String]
        inputLines = lines input

        splitInput :: ([String], [String])
        splitInput = (input1, tail input2) 
            where (input1, input2) = break null inputLines

        parseOrderLine :: String -> (Int, Int)
        parseOrderLine xs = (read num1, (read . tail) num2)  
            where (num1, num2) = break (== '|') xs
        
        parseUpdateLine :: String -> [Int]
        parseUpdateLine = map read . splitOn "," 

isReachable :: Int -> Int -> [(Int, Int)] -> Bool
isReachable x y xs 
    | null reachable     = False
    | y `elem` reachable = True
    | otherwise          = any (\z -> isReachable z y xs) reachable
    where
        reachable :: [Int]
        reachable = map snd . filter ((== x) . fst) $ xs

solveA :: ([(Int, Int)], [[Int]]) -> Int
solveA (order, updates) = sum . map centerElement $ validUpdates
    where
        validUpdates :: [[Int]]
        validUpdates = filter (\xs -> allInOrder (orderOfInterest xs order) xs) updates 

        orderOfInterest :: [Int] -> [(Int, Int)] -> [(Int, Int)]
        orderOfInterest xs = filter (liftA2 (&&) ((`elem` xs) . fst) ((`elem` xs) . snd))

        allInOrder :: [(Int, Int)] -> [Int] -> Bool
        allInOrder order = all (\(x, y) -> isReachable x y order) . liftA2 zip id tail

        centerElement :: [a] -> a
        centerElement xs = xs !! middle
            where middle = length xs `div` 2

mainA :: IO ()
mainA = do
    input <- readFile inputFile
    processed <- return . processInput $ input
    print . solveA $ processed

customCompare :: [(Int, Int)] -> Int -> Int -> Ordering
customCompare order x y
    | x == y = EQ 
    | (x, y) `elem` order = LT 
    | (y, x) `elem` order = GT 
    | otherwise = EQ 

sortWithCustomOrder :: [(Int, Int)] -> [Int] -> [Int]
sortWithCustomOrder order = sortBy (customCompare order)

solveB :: ([(Int, Int)], [[Int]]) -> Int
solveB (order, updates) = sum . map centerElement $ orderedUpdates
    where
        orderedUpdates :: [[Int]]
        orderedUpdates = map (\xs -> sortWithCustomOrder (orderOfInterest xs order) xs) invalidUpdates

        invalidUpdates :: [[Int]]
        invalidUpdates = filter (not . \xs -> allInOrder (orderOfInterest xs order) xs) updates 

        orderOfInterest :: [Int] -> [(Int, Int)] -> [(Int, Int)]
        orderOfInterest xs = filter (liftA2 (&&) ((`elem` xs) . fst) ((`elem` xs) . snd))

        allInOrder :: [(Int, Int)] -> [Int] -> Bool
        allInOrder order = all (\(x, y) -> isReachable x y order) . liftA2 zip id tail

        centerElement :: [a] -> a
        centerElement xs = xs !! middle
            where middle = length xs `div` 2
            
mainB :: IO ()
mainB = do
    input <- readFile inputFile
    processed <- return . processInput $ input
    print . solveB $ processed


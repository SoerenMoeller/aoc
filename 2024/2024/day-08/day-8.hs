import Control.Applicative (liftA2)
import Data.List (nub)

inputFile :: String
inputFile = "day-8-input.txt"

inputFileExample :: String
inputFileExample = "day-8-input-example.txt"

indices :: [[a]] -> [(Int, Int)]
indices xs = [(x, y) | x <- [0..(length . head) xs - 1], y <- [0..length xs - 1]]

matrixAt :: [[a]] -> (Int, Int) -> a
matrixAt mx (x, y) = (mx !! y) !! x

parsePositions :: [String] -> [(Int, Int)] -> [(Char, [(Int, Int)])] -> [(Char, [(Int, Int)])]
parsePositions ls [] res = res
parsePositions ls (pos:xs) res = 
    if matrixAt ls pos == '.'
    then 
        parsePositions ls xs res 
    else 
        let
            char = matrixAt ls pos
            (existing, others) = break ((==) char . fst) res 
        in case others of
            [] -> parsePositions ls xs $ (char, [pos]):res
            ((c, positions):rest) -> parsePositions ls xs (existing ++ ((c, pos:positions):rest))

processInput :: String -> [(Char, [(Int, Int)])]
processInput xs = parsePositions ls (indices ls) []
    where ls = lines xs

inputSize :: String -> (Int, Int)
inputSize = liftA2 (,) length (length . head) . lines

cartesianProduct :: Ord a => [a] -> [(a, a)]
cartesianProduct xs = filter (liftA2 (<) fst snd) $ liftA2 (,) xs xs

vecSub :: Num a => (a, a) -> (a, a) -> (a, a)
vecSub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

vecInv :: Num a => (a, a) -> (a, a)
vecInv (x, y) = (-x, -y)

vecAdd :: Num a => (a, a) -> (a, a) -> (a, a)
vecAdd (x1, y1) (x2, y2) = (x2 + x1, y2 + y1)

isInConstrains :: (Num a, Ord a) => a -> a -> (a, a) -> Bool
isInConstrains h w (x, y) = 0 <= x && x < w && 0 <= y && y < h

solveA :: Int -> Int -> [(Char, [(Int, Int)])] -> Int
solveA h w = length . nub . concatMap (calcPosForFrequency . snd)
    where
        calcPosForFrequency :: [(Int, Int)] -> [(Int, Int)]
        calcPosForFrequency = concatMap (filter (isInConstrains h w) . antinodes) . cartesianProduct
        
        antinodes :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
        antinodes ((x1, y1), (x2, y2)) = 
            let 
                diff = vecSub (x2, y2) (x1, y1)
            in
                [vecSub (x1, y1) diff, vecAdd (x2, y2) diff]

mainA :: IO ()
mainA = do
    input <- readFile inputFile
    (height, width) <- return . inputSize $ input
    processed <- return . processInput $ input
    --print processed
    print . solveA height width $ processed

solveB :: Int -> Int -> [(Char, [(Int, Int)])] -> Int
solveB h w = length . nub . concatMap (calcPosForFrequency . snd)
    where
        calcPosForFrequency :: [(Int, Int)] -> [(Int, Int)]
        calcPosForFrequency = concatMap antinodes . cartesianProduct
        
        antinodes :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
        antinodes ((x1, y1), (x2, y2)) = 
            let 
                (dx, dy) = vecSub (x2, y2) (x1, y1)
            in
                takeWhile (isInConstrains h w) [(x1 - i*dx, y1 - i*dy) | i <- [0..]]
                ++ 
                takeWhile (isInConstrains h w) [(x2 + i*dx, y2 + i*dy) | i <- [0..]]
                
mainB :: IO ()
mainB = do
    input <- readFile inputFile
    (height, width) <- return . inputSize $ input
    processed <- return . processInput $ input
    print . solveB height width $ processed


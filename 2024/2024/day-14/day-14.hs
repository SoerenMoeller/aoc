import Data.List.Split (splitOn)
import Data.Ord (comparing)
import Data.List (minimumBy)

inputFile :: String
inputFile = "day-14-input.txt"

inputFileExample :: String
inputFileExample = "day-14-input-example.txt"

type Position = (Int, Int)
type Velocity = (Int, Int)

parseLine :: String -> (Position, Velocity)
parseLine line = ((read x1, read y1), (read x2, read y2))
  where 
    [pPart, vPart] = splitOn " v=" line  
    [_, pCoords] = splitOn "=" pPart     
    [x1, y1] = splitOn "," pCoords       
    [x2, y2] = splitOn "," vPart         

processInput :: String -> [(Position, Velocity)]
processInput = map parseLine . lines 

walk :: Int -> Int -> Int -> (Position, Velocity) -> Position
walk seconds width height ((x, y), (vx, vy)) = 
    ((x + vx * seconds) `mod` width, (y + vy * seconds) `mod` height)

quadrants :: Int -> Int -> [Position] -> [[Position]]
quadrants width height xs = 
    map (\(opX, opY) -> filter (\(x, y) -> opX x cx && opY y cy) xs) operators
  where 
    operators = [((>), (>)), ((>), (<)), ((<), (>)), ((<), (<))] 
    cx = width `div` 2
    cy = height `div` 2

solveA :: Int -> Int -> Int -> [(Position, Velocity)] -> Int
solveA width height seconds = product
    . map length
    . quadrants width height
    . filter ((&&) <$> (/= (width `div` 2)) . fst <*> (/= (height `div` 2)) . snd)
    . map (walk seconds width height)

mainA :: IO ()
mainA = do
    input <- processInput <$> readFile inputFile
    print . solveA 101 103 100 $ input

minByWeight :: (a -> Double) -> [a] -> a
minByWeight weightFunction = minimumBy (comparing weightFunction) 

simulate103 :: Int -> Int -> [(Position, Velocity)] -> [(Int, [Position])]
simulate103 width height = zip [0..] . map (map fst) . take 103
    . iterate (map (\(p, v) -> (walk 1 width height (p, v), v)))

variance :: [Int] -> Double
variance xs = 
  let n = length xs
      mean = fromIntegral (sum xs) / fromIntegral n
  in sum [(fromIntegral x - mean) ^ 2 | x <- xs] / fromIntegral n

findBestTime :: Int -> Int -> Int -> Int -> Int
findBestTime bx by w h = -- t = bx + inverse(W)*(by-bx)*W
  let wInv = 51 -- precomputed value, inverse of 103 mod H 
      delta = by - bx
      k = (delta * wInv) `mod` h
  in bx + k * w

solveB :: Int -> Int -> [(Position, Velocity)] -> (Int, [Position])
solveB width height xs = (bestTime, treeMap)
  where
    simulation :: [(Int, [Position])]
    simulation = simulate103 width height xs
    
    lowestVarianceX :: (Int, [Position])
    lowestVarianceX = minByWeight (variance . map fst . snd) simulation
    
    lowestVarianceY :: (Int, [Position])
    lowestVarianceY = minByWeight (variance . map snd . snd) simulation

    bestTime :: Int
    bestTime = findBestTime (fst lowestVarianceX) (fst lowestVarianceY) width height

    treeMap :: [Position]
    treeMap = map (walk bestTime width height) xs

pixelsToString :: Int -> Int -> [Position] -> String
pixelsToString width height xs = 
    concatMap (\x -> map (\y -> if (y, x) `elem` xs then 'X' else ' ') 
    [0..width-1] ++ "\n") [0..height-1]

mainB :: IO ()
mainB = do
    input <- processInput <$> readFile inputFile
    (result, tree) <- return . solveB 101 103 $ input
    print result
    putStr . pixelsToString 101 103 $ tree


import Data.Set (Set)
import Data.Bifunctor (bimap)
import qualified Data.Set as Set
import Control.Applicative (liftA2)
import Data.Fixed (mod')
import Data.List ((\\))
import Debug.Trace (trace)

inputFile :: String
inputFile = "day-12-input.txt"

inputFileExample :: String
inputFileExample = "day-12-input-example.txt"

indices :: [[a]] -> Set (Int, Int)
indices xs = Set.fromList $ liftA2 (,) [0..(length . head) xs - 1] [0..length xs - 1]

matrixAt :: [[a]] -> (Int, Int) -> a
matrixAt mx (x, y) = (mx !! y) !! x

inBounds :: [[a]] -> (Int, Int) -> Bool
inBounds mx (x, y) = 0 <= x && x < width && 0 <= y && y < height
  where
    width  = length . head $ mx
    height = length mx

neighbors :: [[a]] -> (Int, Int) -> [(Int, Int)]
neighbors mx (x, y) = filter (inBounds mx) . map (bimap (x +) (y +)) $ [(0, 1), (0, -1), (1, 0), (-1, 0)]

floodFill :: Set (Int, Int) -> [String] -> [(Int, Int)] -> (Set (Int, Int), (Int, Int))
floodFill unvisited _ [] = (unvisited, (0, 0))
floodFill unvisited mx ((x, y):xs) 
    | Set.notMember (x, y) unvisited = floodFill unvisited mx xs
    | otherwise = (newUnvisited, (1 + area, 4 - length correctNeighbors + perimeter))
      where
        symbol :: Char
        symbol = matrixAt mx (x, y)

        correctNeighbors :: [(Int, Int)] 
        correctNeighbors = filter ((symbol ==) . matrixAt mx) . neighbors mx $ (x, y)

        (newUnvisited, (area, perimeter)) = 
          floodFill (Set.delete (x, y) unvisited) mx (correctNeighbors ++ xs)

collectAllRegions :: [String] -> [(Int, Int)]
collectAllRegions mx = collect $ indices mx
  where
    collect :: Set (Int, Int) -> [(Int, Int)]
    collect unvisited 
        | Set.null unvisited = []
        | otherwise = result:collect newUnvisited
          where
            (newUnvisited, result) = floodFill unvisited mx [Set.findMin unvisited]

solveA :: [String] -> Int
solveA = sum . map (uncurry (*)) . collectAllRegions

mainA :: IO ()
mainA = do
    input <- readFile inputFile
    processed <- return . lines $ input
    print . solveA $ processed

floodFillB :: Set (Int, Int) -> [String] -> [(Int, Int)] -> (Set (Int, Int), [(Int, Int)])
floodFillB unvisited _ [] = (unvisited, [])
floodFillB unvisited mx ((x, y):xs) 
    | Set.notMember (x, y) unvisited = floodFillB unvisited mx xs
    | otherwise = (newUnvisited, (x, y):ys)
      where
        symbol :: Char
        symbol = matrixAt mx (x, y)

        correctNeighbors :: [(Int, Int)] 
        correctNeighbors = filter ((symbol ==) . matrixAt mx) . neighbors mx $ (x, y)

        (newUnvisited, ys) = floodFillB (Set.delete (x, y) unvisited) mx (correctNeighbors ++ xs)

collectAllRegionsB :: [String] -> [[(Int, Int)]]
collectAllRegionsB mx = collect $ indices mx
  where
    collect :: Set (Int, Int) -> [[(Int, Int)]]
    collect unvisited 
        | Set.null unvisited = []
        | otherwise = result:collect newUnvisited
         where
            (newUnvisited, result) = floodFillB unvisited mx [Set.findMin unvisited]
getEdges :: [(Int, Int)] -> [((Float, Float), (Float, Float))]
getEdges xs = concatMap helper xs
  where
    helper :: (Int, Int) -> [((Float, Float), (Float, Float))]
    helper (x, y) = map (\(dx, dy) -> ((dx, dy), (dx - fromIntegral x, dy - fromIntegral y)))
      . map (\(dx, dy) -> (fromIntegral (x + dx) / 2, fromIntegral (y + dy) / 2)) 
      . filter (`notElem` xs) 
      $ [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

countSides :: [((Float, Float), (Float, Float))] -> Int
countSides [] = 0
countSides (((x, y), d):xs) = 1 + countSides newSides
  where
    dir = if mod' x 1 == 0 then [(-1, 0), (1, 0)] else [(0, -1), (0, 1)]
    walks = map (\(dx, dy) -> [((x+dx*i, y+dy*i), d) | i <- [1..]]) dir
    validWalks = map (takeWhile (`elem` xs)) walks
    newSides = (xs \\ head validWalks) \\ last validWalks

toCost :: [(Int, Int)] -> Int
toCost xs = (*) (length xs) . countSides . getEdges $ xs

solveB :: [String] -> Int
solveB = sum . map toCost . collectAllRegionsB

mainB :: IO ()
mainB = do
    input <- readFile inputFile
    processed <- return . lines $ input
    print . solveB $ processed


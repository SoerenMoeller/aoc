import System.CPUTime
import Text.Printf

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (elemIndex, nub)
import Data.Maybe (fromJust)
import Debug.Trace (trace)
import Data.Bifunctor (bimap)

type Position  = (Int, Int)
type ParentMap = Map Position Position

dfs :: (Position -> [Position])
    -> (Position -> Bool)
    -> [(Position, ParentMap)]
    -> [Position]
dfs successors isEnd [] = error "No path found"
dfs successors isEnd ((current, parent):xs)
    | isEnd current = reconstructPath current parent
    | otherwise     = dfs' (successors' ++ xs)
      where
        dfs'    = dfs successors isEnd
        successors' = filter ((`Map.notMember` parent) . fst)
            . map (\pos -> (pos, Map.insert pos current parent))
            $ successors current

reconstructPath :: Position -> ParentMap -> [Position]
reconstructPath pos parent = reverse . retrace $ pos
  where
    retrace p = case Map.lookup p parent of
      Nothing -> [p]
      Just prev -> p : retrace prev

matrixAt :: [[a]] -> Position -> a
matrixAt mx (x, y) = (mx !! y) !! x

performCheat :: [String]
            -> Map Position Int
            -> [Position]
            -> Int
            -> Position
            -> [Int]
performCheat mx indexMap path cheatDist (x, y) =
    map (\(i, d) -> i - currentIndex - d)
    . filter ((currentIndex <) . fst)
    . map (\(p, d) -> (fromJust $ Map.lookup p indexMap, d))
    . filter (flip Map.member indexMap . fst)
    $ generateManhattanPositions (x, y) cheatDist
  where
    currentIndex = fromJust $ Map.lookup (x, y) indexMap

solve :: [String] -> Int -> [Int]
solve mx cheatDist =
    concatMap (filter (>= 100) . performCheat mx indexMap path cheatDist) path
  where
    diff :: [Position] -> Int
    diff p2 = length path - length p2 - 1

    indexMap = Map.fromList . zip path $ [0..]

    path = dfs successors isEnd [(start, Map.empty)]
    states = (,) <$> [0..(length . head $ mx) - 1] <*> [0..length mx - 1]
    [start] = filter ((== 'S') . matrixAt mx) states
    isEnd pos = matrixAt mx pos == 'E'
    successors (x, y) = filter ((/= '#') . matrixAt mx) [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

generateManhattanPositions :: Position -> Int -> [(Position, Int)]
generateManhattanPositions (x, y) num = 
    [ ((x + dx, y + dy), d)
    | d <- [1..num]  
    , dx <- [-d..d]  
    , dy <- [-d..d]  
    , abs dx + abs dy == d 
    ]

main :: IO ()
main = do
    let inputData        = "day-20-input.txt"
    let inputDataExample = "day-20-input-example.txt"

    startTotal <- getCPUTime
    input <- lines <$> readFile inputData

    start <- getCPUTime
    putStrLn . (++) "Problem 1:  " . show . length . solve input $ 2
    end <- getCPUTime
    printf "Time:       %.3fs\n\n" $ toSeconds start end

    start <- getCPUTime
    putStrLn . (++) "Problem 2:  " . show . length . solve input $ 20
    end <- getCPUTime
    printf "Time:       %.3fs\n\n" $ toSeconds start end

    endTotal <- getCPUTime
    printf "Total Time: %.3fs\n" $ toSeconds startTotal endTotal
  where
    toSeconds :: Integer -> Integer -> Double
    toSeconds start end = fromIntegral (end - start) / (10^12)

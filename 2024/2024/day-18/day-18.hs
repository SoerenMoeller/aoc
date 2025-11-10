import System.CPUTime
import Text.Printf

import Data.Bifunctor (bimap)
import Data.Heap (MinHeap)
import qualified Data.Heap as Heap
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Map (Map)
import qualified Data.Map as Map

type Position = (Int, Int)

inputData :: (Int, Int, String)
inputData = (71, 1024, "day-18-input.txt")

inputDataExample :: (Int, Int, String)
inputDataExample = (7, 12, "day-18-input-example.txt")

processInput :: String -> [(Int, Int)]
processInput = map (bimap read (read . tail) . break (== ',')) . lines

directions :: [Position]
directions = [(1, 0), (-1, 0), (0, 1), (0, -1)]

isInBounds :: Int -> Position -> Bool
isInBounds max (x, y) = 0 <= x && 0 <= y && x < max && y < max

manhattan :: Position -> Position -> Int
manhattan (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

reconstructPath :: Map Position Position -> Position -> [Position]
reconstructPath parents pos =
    case Map.lookup pos parents of
        Nothing   -> [pos]
        Just prev -> reconstructPath parents prev ++ [pos]

aStar :: Int 
      -> Set Position 
      -> MinHeap (Int, Position) 
      -> Set Position 
      -> Map Position Int 
      -> Map Position Position 
      -> Maybe [Position]
aStar dim invalid open closed costs parents
    | Heap.isEmpty open = Nothing
    | isDone            = Just $ reconstructPath parents (dim - 1, dim - 1)
    | otherwise         = aStar dim invalid updatedOpenSet newClosedSet updatedCosts updatedParents
      where
        (fCost, (x, y)) = fromJust $ Heap.viewHead open
        gCost           = fromJust $ Map.lookup (x, y) costs
        isDone          = (x, y) == (dim - 1, dim - 1)

        isSuccValid pos = notElem pos invalid && notElem pos closed && isInBounds dim pos && betterCost pos
        betterCost  pos = pos `Map.notMember` costs || gCost + 1 < fromJust (Map.lookup pos costs)

        successors     = filter isSuccValid . map (bimap (+ x) (+ y)) $ directions
        newClosedSet   = Set.insert (x, y) closed
        updatedOpenSet = foldr 
            (\s heap -> Heap.insert (gCost + 1 + manhattan s (dim - 1, dim - 1), s) heap) 
            (fromJust $ Heap.viewTail open) 
            successors
        updatedCosts   = foldr (\s acc -> Map.insertWith min s (gCost + 1) acc) costs successors
        updatedParents = foldr (\s acc -> Map.insertWith (\_ old -> old) s (x, y) acc) parents successors

solveA :: Int -> [Position] -> [Position]
solveA dim xs = fromJust $ aStar
    dim
    (Set.fromList xs)
    (Heap.singleton (manhattan (0, 0) (dim - 1, dim - 1), (0, 0)))
    Set.empty
    (Map.singleton (0, 0) 0)
    Map.empty

solveB :: Int -> Int -> Int -> [Position] -> Int
solveB dim left right xs 
    | isNothing r1 && isNothing r2 = solveB dim left      (mid - 1) xs
    | isJust r1 && isJust r2       = solveB dim (mid + 1) right     xs
    | otherwise = mid + 1
  where
    mid = (left + right) `div` 2
    [r1, r2] = map applyWithBytes [mid, mid + 1]
    
    applyWithBytes b = aStar
        dim
        (Set.fromList $ take b xs)
        (Heap.singleton (manhattan (0, 0) (dim - 1, dim - 1), (0, 0)))
        Set.empty
        (Map.singleton (0, 0) 0)
        Map.empty

printTime :: Bool -> Integer -> Integer -> IO () 
printTime True start end  = printf "Total Time: %.3fs\n" (fromIntegral (end - start) / (10^12) ::  Double)
printTime False start end = printf "Time:       %.3fs\n\n" (fromIntegral (end - start) / (10^12) ::  Double)

main :: IO ()
main = do
    startTotal <- getCPUTime
    let (dim, bytes, fileName) = inputData
    processed <- processInput <$> readFile fileName 

    start <- getCPUTime
    putStrLn . (++) "Problem 1:  " . show . pred . length . solveA dim 
        $ take bytes processed
    end <- getCPUTime
    printTime False start end

    start <- getCPUTime
    putStrLn . (++) "Problem 2:  " . show . (processed !!) . flip (-) 1 
        . solveB dim 0 (length processed - 1) $ processed

    end <- getCPUTime
    printTime False start end

    endTotal <- getCPUTime
    printTime True startTotal endTotal
{-# LANGUAGE TupleSections #-}

import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Algorithm.Search (aStar, dijkstra)
import Data.Maybe (fromJust, isJust)
import Debug.Trace (trace)
import Data.Bifunctor (bimap)
import Data.List (nub)

import Data.PSQueue (PSQ, Binding((:->)))
import qualified Data.PSQueue as PSQ

inputFile :: String
inputFile = "day-16-input.txt"

inputFileExample :: String
inputFileExample = "day-16-input-example.txt"

type Position = (Int, Int)
type Direction = (Int, Int)
type Node = (Position, Direction)
type GraphMap = Map Node (Set Node)
type CostMap  = Map Node (Map Node Int)
data Graph = Graph { start      :: Node
                   , edges      :: GraphMap
                   , costs      :: CostMap
                   , end        :: [Node] }
    deriving Show

matrixAt :: [[a]] -> Position -> a
matrixAt mx (x, y) = (mx !! y) !! x

directions :: [Direction]
directions = [(0, 1), (0, -1), (1, 0), (-1, 0)]

buildEdges :: [Node] -> (Position -> Bool) -> [String] -> GraphMap -> GraphMap
buildEdges [] _ _ m = m
buildEdges (((x, y), dir):xs) isInBounds mx m
    | ((x, y), dir) `Map.member` m = buildEdges xs isInBounds mx m
    | otherwise                    = buildEdges (neighbors ++ xs) isInBounds mx m'
  where
    neighbors :: [Node]
    neighbors = map (\(nx, ny) -> ((nx, ny), (x-nx, y-ny))) 
        . filter ((/= '#') . matrixAt mx)
        . filter isInBounds 
        . map (\(x2, y2) -> (x + x2, y + y2)) $ directions 

    m' :: GraphMap
    m' = Map.insert ((x, y), dir) (Set.fromList neighbors) m

needsTurn :: Position -> Position -> Bool
needsTurn (x1, y1) (x2, y2) = x1 == x2 || y1 == y2

buildCosts :: GraphMap -> CostMap
buildCosts = Map.mapWithKey mapAdjacence
  where
    mapAdjacence :: Node -> Set Node -> Map Node Int 
    mapAdjacence (_, (dx1, dy1)) = 
        Map.fromList . map calculateCost . Set.toList 
      where
        calculateCost n@(_, (dx2, dy2)) 
          | (dx1, dy1) == (0, 0)     = (n, 1)            
          | (dx1, dy1) == (dx2, dy2) = (n, 1)       
          | otherwise                = (n, 1001)   
        
arrayToGraph :: [String] -> Graph
arrayToGraph mx = 
    Graph {
        start = (start, (0, 0)), 
        edges = edges,
        costs = costs,
        end   = map (end,) directions
    }
  where 
    (width, height) = (length . head $ mx, length mx)
  
    indices :: [Position] 
    indices = (,) <$> [0..width - 1] <*> [0..height - 1]
     
    start :: Position
    start = head . filter ((== 'S') . matrixAt mx) $ indices

    end :: Position
    end = head . filter ((== 'E') . matrixAt mx) $ indices

    isInBounds :: Position -> Bool
    isInBounds (x, y) = 0 <= x && 0 <= y && x < width && y < height

    edges :: GraphMap
    edges = buildEdges [(start, (0, 0))] isInBounds mx Map.empty
    
    costs :: CostMap
    costs = buildCosts edges
    
processInput :: String -> Graph
processInput =  arrayToGraph . lines

assumeCost :: Node -> Node -> Int
assumeCost ((x1, y1),_) ((x2, y2),_) = abs (x2 - x1) + abs (y2 - y1)

solveA :: Graph -> Maybe (Int, [Node])
solveA (Graph start edges costs end) = 
    aStar 
        (Set.toList . fromJust . (`Map.lookup` edges))
        (\s1 s2 -> fromJust . Map.lookup s2 . fromJust . Map.lookup s1 $ costs)
        (assumeCost $ head end)
        (`elem` end)
        start
        
mainA :: IO ()
mainA = do
    input <- readFile inputFile
    processed <- return . processInput $ input
    print . solveA $ processed

-- Should have implemented my own search..

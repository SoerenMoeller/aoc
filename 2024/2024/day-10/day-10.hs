import Data.Char (digitToInt)
import Control.Applicative (liftA2)
import Data.Maybe (fromJust)
import Data.List (nub)

inputFile :: String
inputFile = "day-10-input.txt"

inputFileExample :: String
inputFileExample = "day-10-input-example.txt"

data Node = Node { x :: Int, y :: Int, val :: Int} deriving (Eq, Show) 
type Graph = [(Node, [Node])]

indices :: [[a]] -> [(Int, Int)]
indices xs = [(x, y) | x <- [0..(length . head) xs - 1], y <- [0..length xs - 1]]

matrixAt :: Eq a => [[a]] -> Int -> Int -> a
matrixAt mx x y = (mx !! y) !! x

inBounds :: Int -> Int -> Int -> Int -> Bool
inBounds width height x y = 0 <= x && x < width && 0 <= y && y < height

nodeEdges :: [[Int]] -> Node -> [Node]
nodeEdges mx (Node x y 9) = []
nodeEdges mx (Node x y val) = 
  let 
    relNeighbors   = [(0, 1), (0, -1), (1, 0), (-1, 0)]
    absNeighbors   = map (liftA2 (,) ((+) x . fst) ((+) y . snd)) relNeighbors
    realNeighbors  = filter (uncurry (inBounds ((length . head) mx) (length mx))) absNeighbors
    validNeighbors = filter ((==) (val + 1) . uncurry (matrixAt mx)) realNeighbors
  in
    map (\(x1, x2) -> Node x1 x2 (matrixAt mx x1 x2)) validNeighbors

matrixToGraph :: [[Int]] -> Graph
matrixToGraph mx = map (liftA2 (,) id (nodeEdges mx)) nodes
  where
    nodes :: [Node]
    nodes =  map (\(x, y) -> Node x y (matrixAt mx x y)) $ indices mx

processInput :: String -> Graph
processInput = matrixToGraph . map (map digitToInt) . lines 

successor :: Graph -> Node -> [Node]
successor g n = fromJust $ lookup n g

dfs :: Graph -> Int -> [Node] -> [Node]
dfs _ 9    xs = xs
dfs g step xs = dfs g (step + 1) . nub . concatMap (successor g) $ xs

solveA :: Graph -> Int
solveA g = sum . map (length . dfs g 0 . return) $ pits
  where
    pits :: [Node]
    pits = filter ((==) 0 . val) . map fst $ g

mainA :: IO ()
mainA = do
    input <- readFile inputFile
    processed <- return . processInput $ input
    print . solveA $ processed

calcPaths :: Graph -> [Node] -> [[Node]]
calcPaths g (x:xs) = map (: (x:xs)) (successor g x) 

dfsB :: Graph -> Int -> [[Node]] -> [[Node]]
dfsB _ 9    xs = nub xs
dfsB g step xs = dfsB g (step + 1) . concatMap (calcPaths g) $ xs

solveB :: Graph -> Int
solveB g = sum . map (length . dfsB g 0 . return . return) $ pits
  where
    pits :: [Node]
    pits = filter ((==) 0 . val) . map fst $ g

mainB :: IO ()
mainB = do
    input <- readFile inputFile
    processed <- return . processInput $ input
    print . solveB $ processed


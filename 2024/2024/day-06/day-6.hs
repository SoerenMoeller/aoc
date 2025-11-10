import Prelude hiding (Left, Right)
import Data.List (transpose, nub, intercalate)
import Data.Maybe (isJust, fromJust)
import Control.Applicative (liftA2)

inputFile :: String
inputFile = "day-6-input.txt"

data Cell
    = Obstacle   
    | Up         
    | Down       
    | Left  
    | Right
    | Clear      
    deriving (Eq, Show)

charToCell :: Char -> Cell
charToCell '#' = Obstacle
charToCell '^' = Up
charToCell 'v' = Down
charToCell '<' = Left
charToCell '>' = Right
charToCell '.' = Clear

cellToChar :: Cell -> Char
cellToChar Obstacle = '#'
cellToChar Up       = '^'
cellToChar Clear    = '.'

indicesOf :: Eq a => a -> [a] -> [Int]
indicesOf x = map fst . filter ((== x) . snd) . zip [0..]

binarySearch :: Ord a => a -> [a] -> Int
binarySearch x xs = binarySearchHelper x xs 0 $ length xs - 1
    where
        binarySearchHelper :: Ord a => a -> [a] -> Int -> Int -> Int
        binarySearchHelper x xs low high 
            | low > high     = low
            | x <= xs !! mid = binarySearchHelper x xs low $ mid - 1
            | otherwise      = binarySearchHelper x xs (mid + 1) high
            where mid = (low + high) `div` 2

matrixWidth :: [[a]] -> Int
matrixWidth = length . head

matrixHeight :: [[a]] -> Int
matrixHeight = length 

indices :: [[a]] -> [(Int, Int)]
indices xs = [(x,y) | y <- [0..matrixWidth xs - 1], x <- [0..matrixHeight xs - 1]] 

matrixAt :: [[a]] -> (Int, Int) -> a
matrixAt xs (x, y) = (xs !! y) !! x

isGuard :: [[Cell]] -> (Int, Int) -> Bool
isGuard mx pos = matrixAt mx pos `elem` [Up, Left, Right, Down]

solveA :: [[Cell]] -> Int -- [(Int, Int)]
solveA mx = length . nub . (:) startPosition $ solveHelper startPosition Up
--solveA mx = startPosition : solveHelper startPosition Up
    where         
        rowObstacles :: [[Int]]
        rowObstacles = map (indicesOf Obstacle) mx

        colObstacles :: [[Int]]
        colObstacles = map (indicesOf Obstacle) $ transpose mx

        startPosition :: (Int, Int)
        startPosition = head . filter (isGuard mx) $ indices mx 

        solveHelper :: (Int, Int) -> Cell -> [(Int, Int)]
        solveHelper (x, y) Up  
            | null col || pos == 0 && head col > y = [(x, ny) | ny <- reverse [0..y-1]]
            | otherwise = [(x, ny) | ny <- reverse [endPos..y-1]] ++ solveHelper (x, endPos) Right
            where 
                col = colObstacles !! x
                pos = binarySearch y col
                endPos = if pos == 0 then head col else col !! (pos - 1) + 1
        solveHelper (x, y) Down
            | null col || pos == length col = [(x, ny) | ny <- [y+1..length mx - 1]]
            | otherwise = [(x, ny) | ny <- [y+1..endPos]] ++ solveHelper (x, endPos) Left
            where 
                col = colObstacles !! x
                pos = binarySearch y col
                endPos = col !! pos - 1
        solveHelper (x, y) Left
            | null row || pos == 0 && head row > x = [(nx, y) | nx <- reverse [0..x-1]]
            | otherwise = [(nx, y) | nx <- reverse [endPos..x-1]] ++ solveHelper (endPos, y) Up
            where 
                row = rowObstacles !! y
                pos = binarySearch x row
                endPos = if pos == 0 then head row else row !! (pos - 1) + 1
        solveHelper (x, y) Right
            | null row || pos == length row = [(nx, y) | nx <- [x+1..(length . head) mx - 1]]
            | otherwise = [(nx, y) | nx <- [x+1..endPos]] ++ solveHelper (endPos, y) Down
            where 
                row = rowObstacles !! y
                pos = binarySearch x row
                endPos = row !! pos - 1 

nextDirection :: Cell -> Cell
nextDirection Left = Up
nextDirection Up = Right
nextDirection Right = Down
nextDirection Down = Left

mainA :: IO ()
mainA = do
    input <- readFile inputFile
    processed <- return . map (map charToCell) . lines $ input
    print . solveA $ processed

addObstacle :: [[Cell]] -> [[[Cell]]]
addObstacle mx = map (replaceCell mx) . filter ((==) Clear . matrixAt mx) $ indices mx

replaceCell :: [[Cell]] -> (Int, Int) -> [[Cell]]
replaceCell mx (i, j) = 
    take j mx 
    ++ [replaceInRow (mx !! j) i] 
    ++ drop (j + 1) mx

replaceInRow :: [Cell] -> Int -> [Cell]
replaceInRow row j = 
    take j row 
    ++ [Obstacle] 
    ++ drop (j + 1) row

solveB :: [[Cell]] -> Int
solveB mx = length . filter (solveHelper [] startPosition Up) $ addObstacle mx
--solveB mx = solveHelper [] startPosition Up $ addObstacle mx !! 70
    where         
        rowObstacles :: [[Cell]] -> [[Int]]
        rowObstacles = map $ indicesOf Obstacle

        colObstacles :: [[Cell]] -> [[Int]]
        colObstacles = map (indicesOf Obstacle) . transpose

        startPosition :: (Int, Int)
        startPosition = head . filter (isGuard mx) $ indices mx 

        solveHelper :: [(Int, Int, Cell)] -> (Int, Int) -> Cell -> [[Cell]] -> Bool
        solveHelper corners (x, y) Up mx
            | null col || pos == 0 && head col > y = False
            | (x, y, Up) `elem` corners = True
            | otherwise = solveHelper ((x, y, Up):corners) (x, endPos) Right mx
            where 
                col = colObstacles mx !! x
                pos = binarySearch y col
                endPos = if pos == 0 then head col else col !! (pos - 1) + 1
        solveHelper corners (x, y) Down mx
            | null col || pos == length col = False
            | (x, y, Down) `elem` corners = True
            | otherwise = solveHelper ((x, y, Down):corners) (x, endPos) Left mx
            where 
                col = colObstacles mx !! x
                pos = binarySearch y col
                endPos = col !! pos - 1
        solveHelper corners (x, y) Left mx
            | null row || pos == 0 && head row > x = False
            | (x, y, Left) `elem` corners = True
            | otherwise = solveHelper ((x, y, Left):corners) (endPos, y) Up mx
            where 
                row = rowObstacles mx !! y
                pos = binarySearch x row
                endPos = if pos == 0 then head row else row !! (pos - 1) + 1
        solveHelper corners (x, y) Right mx
            | null row || pos == length row = False
            | (x, y, Right) `elem` corners = True
            | otherwise = solveHelper ((x, y, Right):corners) (endPos, y) Down mx
            where 
                row = rowObstacles mx !! y
                pos = binarySearch x row
                endPos = row !! pos - 1 
                
mainB :: IO ()
mainB = do
    input <- readFile inputFile
    processed <- return . map (map charToCell) . lines $ input
    --printMatrix $ map (map cellToChar) processed
    --printMatrix $ (map (map (map cellToChar)) . addObstacle) processed !! 70
    print . solveB $ processed


printMatrix :: Show a => [[a]] -> IO ()
printMatrix mx = putStrLn (unlines (map (unwords . map show) mx))


import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (foldl')
import Prelude hiding (Left, Right)
import Data.Maybe (fromJust, isJust)
import Control.Monad (forM_)

inputFile :: String
inputFile = "day-15-input.txt"

inputFileExample :: String
inputFileExample = "day-15-input-example.txt"

data Layout = Layout {  roboPos :: Position, 
                        walls :: Set Position, 
                        obstacles :: Set Position }
type Move = (Int, Int)
type Position = (Int, Int)

matrixAt :: [[a]] -> (Int, Int) -> a
matrixAt mx (x, y) = (mx !! y) !! x

parseLayout :: [String] -> Layout
parseLayout mx = Layout { 
    roboPos = head $ searchChar '@', 
    walls = Set.fromList $ searchChar '#', 
    obstacles = Set.fromList $ searchChar 'O' 
  }
  where
    (width, height) = (length . head $ mx, length mx)
    
    indices :: [Position]
    indices = (,) <$> [0..width - 1] <*> [0..height - 1]

    searchChar :: Char -> [Position]
    searchChar c = filter ((== c) . matrixAt mx) indices

toMove :: Char -> Move
toMove '^' = (0, -1)
toMove '<' = (-1, 0)
toMove '>' = (1, 0)
toMove 'v' = (0, 1)

processInput :: String -> (Layout, [Move])
processInput input =(parseLayout layout, map toMove . concat $ moves) 
    where (layout, _ : moves) = break null . lines $ input

add2 :: (Int, Int) -> (Int, Int) -> (Int, Int)
add2 (x, y) (u, v) = (x+u, y+v)

mult2 :: (Int, Int) -> (Int, Int)
mult2 (x, y) = (x * 2, y * 2)

simulateSteps :: Layout -> [Move] -> Layout
simulateSteps = foldl' simulateStep

boxes :: Position -> Move -> Set Position -> [Position]
boxes (x, y) (dx, dy) boxes = 
    takeWhile (`Set.member` boxes) $ [(x+dx*i , y+dy*i) | i <- [0..]]

simulateBoxInWay :: Layout -> Move -> Layout
simulateBoxInWay (Layout robo walls obstacles) move 
    | afterBoxes `Set.member` walls = Layout robo walls obstacles
    | otherwise = Layout step1 walls . Set.delete step1 . Set.insert afterBoxes $ obstacles
  where
    step1 = add2 robo move
    boxesInWay = boxes step1 move obstacles
    afterBoxes = add2 move . last $ boxesInWay

simulateStep:: Layout -> Move -> Layout
simulateStep (Layout robo walls obstacles) move 
    | step1 `Set.member` walls = Layout robo walls obstacles
    | step1 `Set.member` obstacles = simulateBoxInWay (Layout robo walls obstacles) move
    | otherwise = Layout step1 walls obstacles
  where 
    step1 = add2 robo move

solveA :: Layout -> [Move] -> Int
solveA l = sum . map (\(x, y) -> y * 100 + x) . Set.toList . obstacles . simulateSteps l

mainA :: IO ()
mainA = do
    input <- readFile inputFile
    (layout, moves) <- return . processInput $ input
    print $ solveA layout moves

data LayoutB = LayoutB { roboPosB   :: Position, 
                         wallsB     :: Set Position, 
                         obstaclesB :: Set (Position, Position) }

resizeMap :: Layout -> LayoutB
resizeMap (Layout roboPos walls obstacles) = LayoutB updRoboPos updWalls updObstacles
  where 
    mult2X (x, y) = (x * 2, y) 
    updRoboPos    = mult2X roboPos
    walls2        = Set.map mult2X walls
    updWalls      = walls2 `Set.union` Set.map (\(x, y) -> (x + 1, y)) walls2
    obstacles2    = Set.map mult2X obstacles
    updObstacles  = Set.map (\(x, y) -> ((x, y), (x + 1, y))) obstacles2

simulateStepsB :: LayoutB -> [Move] -> LayoutB
simulateStepsB = foldl' simulateStepB

getBox :: Set (Position, Position) -> Position -> Maybe (Position, Position)
getBox boxes pos = if Set.null result then Nothing else Just $ Set.findMin result
  where 
    result = Set.union
        (Set.filter ((== pos) . fst) boxes)
        (Set.filter ((== pos) . snd) boxes)

boxesB :: Move -> Set (Position, Position) -> [(Position, Position)] -> Set (Position, Position) -> [(Position, Position)]
boxesB _    _     []                _       = []
boxesB move boxes ((pos1, pos2):xs) visited 
    | (pos1, pos2) `Set.member` visited = boxesB move boxes xs visited
    | otherwise = (pos1, pos2) : boxesB move boxes (nextBoxes ++ xs) (Set.insert (pos1, pos2) visited)
  where
    nextBoxes = Set.toList . Set.filter 
        (\(posa, posb) -> 
            posa == add2 move pos1 || posa == add2 move pos2 || posb == add2 move pos1 || posb == add2 move pos2)
        $ boxes
        
updateBox :: Move -> [(Position, Position)] -> (Position, Position) -> (Position, Position)
updateBox move toUpdate box@(pos1, pos2)  
    | box `elem` toUpdate = (add2 pos1 move, add2 pos2 move)
    | otherwise           = box

simulateBoxInWayB :: Position -> (Position, Position) -> LayoutB -> Move -> LayoutB
simulateBoxInWayB nextStep box (LayoutB robo walls obstacles) move 
    | hitsWall  = LayoutB robo walls obstacles
    | otherwise = LayoutB nextStep walls . Set.map (updateBox move allBoxes) $ obstacles
  where
    allBoxes = boxesB move obstacles [box] Set.empty
    hitsWall = any (\(pos1, pos2) -> add2 move pos1 `elem` walls || add2 move pos2 `elem` walls) allBoxes

simulateStepB:: LayoutB -> Move -> LayoutB
simulateStepB (LayoutB robo walls obstacles) move 
    | step `Set.member` walls = LayoutB robo walls obstacles
    | isJust nextBoxResult = simulateBoxInWayB step (fromJust nextBoxResult) (LayoutB robo walls obstacles) move
    | otherwise = LayoutB step walls obstacles
  where 
    step = add2 robo move
    nextBoxResult  = getBox obstacles step

printLayoutAt :: Int -> Int -> LayoutB -> Char
printLayoutAt x y (LayoutB pos walls obstacles) 
    | (x, y) == pos                      = '@'
    | (x, y) `Set.member` walls          = '#'
    | (x, y) `Set.member` leftObstacles  = '['
    | (x, y) `Set.member` rightObstacles = ']'
    | otherwise                          = '.'
  where
    leftObstacles = Set.map fst obstacles  
    rightObstacles = Set.map snd obstacles 

printLayout :: LayoutB -> IO ()
printLayout layout = do
    let width = 20
    let height = 10  
    forM_ [0..height-1] $ \y -> do 
        let row = [printLayoutAt x y layout | x <- [0..width-1]]
        putStrLn row 
        
solveB :: LayoutB -> [Move] -> Int
solveB l = sum . map (\((x, y), _) -> y * 100 + x) . Set.toList . obstaclesB . simulateStepsB l

mainB :: IO ()
mainB = do
    input <- readFile inputFile
    (layout, moves) <- return . processInput $ input
    print . solveB (resizeMap layout) $ moves 


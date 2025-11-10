inputFile :: String
inputFile = "day-4-input.txt"

solveA :: [String] -> Int
solveA xs = sum . map (`calcWeigth` xs) . indices $ xs
    
indices :: [[a]] -> [(Int, Int)]
indices xs = [(x,y) | y <- [0..length xs - 1], x <- [0.. (length . head) xs - 1]] 

calcWeigth :: (Int, Int) -> [String] -> Int
calcWeigth pos xs = if matrixAt xs pos == 'X' then checkXmas pos xs else 0

generateValidPaths :: (Int, Int) -> Int -> Int -> Int -> [[(Int, Int)]]
generateValidPaths (x, y) len min max = filter isValidPath paths
    where
        paths :: [[(Int, Int)]]
        paths = map (generatePath (x, y) len) directionsList
        
        isValidPath :: [(Int, Int)] -> Bool
        isValidPath path = all inBounds path && length path == len

        inBounds :: (Int, Int) -> Bool
        inBounds (i, j) = i >= min && i < max && j >= min && j < max

        directionsList :: [(Int, Int)]
        directionsList = [(1, 0), (0, 1), (-1, 0), (0, -1),  (1, 1), (1, -1), 
                          (-1, 1), (-1, -1)] 

generatePath :: (Int, Int) -> Int -> (Int, Int) -> [(Int, Int)]
generatePath (x, y) len (dx, dy) = take len [(x + i * dx, y + i * dy) 
                                             | i <- [0..]]

matrixAt :: [[a]] -> (Int, Int) -> a
matrixAt matrix (x, y) = (matrix !! y) !! x

checkXmas :: (Int, Int) -> [String] -> Int
checkXmas pos matrix = length . filter (== "XMAS") . map (map (matrixAt matrix)) 
                              . generateValidPaths pos 4 0 $ length matrix

mainA :: IO ()
mainA = do
    input <- readFile inputFile
    processed <- return . lines $ input
    print . solveA $ processed

solveB :: [String] -> Int
solveB xs = sum . map (`calcWeigthB` xs) . indices $ xs

calcWeigthB :: (Int, Int) -> [String] -> Int
calcWeigthB pos xs = if matrixAt xs pos == 'A' then checkMas pos xs else 0

generateValidPathsB :: (Int, Int) -> Int -> Int -> Int -> [[(Int, Int)]]
generateValidPathsB (x, y) len min max = filter isValidPath paths
    where
        paths :: [[(Int, Int)]]
        paths = map (generatePath (x, y) len) directionsList
        
        isValidPath :: [(Int, Int)] -> Bool
        isValidPath path = all inBounds path && length path == len

        inBounds :: (Int, Int) -> Bool
        inBounds (i, j) = i >= min && i < max && j >= min && j < max

        directionsList :: [(Int, Int)]
        directionsList = [(1, 1), (1, -1), (-1, 1), (-1, -1)] 

        generatePath :: (Int, Int) -> Int -> (Int, Int) -> [(Int, Int)]
        generatePath (x, y) len (dx, dy) = take len [(x + i * dx, y + i * dy) 
                                                     | i <- [-1, 0, 1]]

checkMas :: (Int, Int) -> [String] -> Int
checkMas pos matrix = if masAmount == 2 then 1 else 0
    where
        masAmount :: Int
        masAmount = length . filter (== "MAS") . map (map (matrixAt matrix)) 
                           . generateValidPathsB pos 3 0 $ length matrix
        

mainB :: IO ()
mainB = do
    input <- readFile inputFile
    processed <- return . lines $ input
    print . solveB $ processed


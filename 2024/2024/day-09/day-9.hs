import Data.Char (digitToInt)
import Debug.Trace (trace)

inputFile :: String
inputFile = "day-9-input.txt"

data MemType = Free { size :: Int} 
             | Occupied { size :: Int, uid :: Int }
    deriving (Show, Eq)

processInput :: String -> [MemType]
processInput = zipWith toMemType [0..]
  where
    toMemType :: Int -> Char -> MemType
    toMemType index digit 
        | even index = Occupied {size = digitToInt digit, uid = index `div` 2}
        | otherwise  = Free {size = digitToInt digit}

checksumBlock :: Int -> MemType -> Int
checksumBlock _ (Free _) = 0
checksumBlock idx (Occupied size uid) = sum . map (* uid) $ [idx..idx + size - 1]

solveA :: Int -> [MemType] -> [MemType] ->  Int
solveA _ []          _  = 0
solveA idx (Occupied size uid:xs) (Occupied sizeBuff uidBuff:ys) 
    | uid == uidBuff = checksumBlock idx (Occupied sizeBuff uid)
    | otherwise      = checksumBlock idx (Occupied size uid) 
        + solveA (idx + size) xs (Occupied sizeBuff uidBuff:ys)
solveA idx (Free freeSize:xs) (Occupied occupiedSize uid:ys) 
    | freeSize < occupiedSize  = 
        checksumBlock idx (Occupied freeSize uid) 
        + solveA (idx + freeSize) 
            (init xs ++ [Occupied (occupiedSize - freeSize) uid]) 
            (Occupied (occupiedSize - freeSize) uid:ys)
    | freeSize == occupiedSize = 
        checksumBlock idx (Occupied freeSize uid) 
        + solveA (idx + freeSize) (init xs) ys
    | freeSize > occupiedSize  = 
        checksumBlock idx (Occupied occupiedSize uid) 
        + solveA (idx + occupiedSize) 
            (Free (freeSize - occupiedSize):init xs) ys
        
isOccupied :: MemType -> Bool
isOccupied (Occupied _ _) = True
isOccupied _              = False

mainA :: IO ()
mainA = do
    input <- init <$> readFile inputFile
    processed <- return . processInput $ input
    buffer <- return . reverse . filter isOccupied $ processed
    print . solveA 0 processed $ buffer

mergeFree :: [MemType] -> [MemType]
mergeFree [] = []
mergeFree [x] = [x]
mergeFree (Free s1 : Free s2 : xs) = mergeFree (Free (s1 + s2) : xs) 
mergeFree (x:xs) = x : mergeFree xs 

tryMove :: MemType -> [MemType] -> Maybe [MemType]
tryMove _ [] = Nothing
tryMove (Occupied s1 i1) (Occupied s2 i2:xs) = (Occupied s2 i2 :) <$> tryMove (Occupied s1 i1) xs
tryMove (Occupied s1 i1) (Free s2:xs) 
    | s1 == s2  = Just $ Occupied s1 i1 : xs
    | s1 < s2   = Just $ Occupied s1 i1 : Free (s2 - s1) : xs
    | otherwise = (Free s2 :) <$> tryMove (Occupied s1 i1) xs

moveFiles :: [MemType] -> [MemType] -> [MemType] 
moveFiles xs []          = xs
moveFiles xs (Occupied s i:ys) = case tryMove (Occupied s i) prefix of
    Nothing             -> moveFiles xs ys
    Just updatedPrefix  -> moveFiles (mergeFree (updatedPrefix ++ [Free s] ++ suffix)) ys
  where
    (prefix, e:suffix) = break (== Occupied s i) xs
    

solveB :: [MemType] -> Int
solveB xs = solveHelper 0 movedMemList
  where 
    movedMemList :: [MemType]
    movedMemList = moveFiles xs $ reverse . filter isOccupied $ xs
  
    solveHelper :: Int -> [MemType] -> Int
    solveHelper _   [] = 0
    solveHelper idx (Free s:xs) = solveHelper (idx + s) xs
    solveHelper idx (Occupied s i:xs) = checksumBlock idx (Occupied s i) + solveHelper (idx + s) xs
    

mainB :: IO ()
mainB = do
    input <- init <$> readFile inputFile
    processed <- return . processInput $ input
    buffer <- return . reverse . filter isOccupied $ processed
    print . solveB $ processed 

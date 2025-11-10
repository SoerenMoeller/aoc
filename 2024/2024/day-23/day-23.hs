{-# LANGUAGE TupleSections #-}
import System.CPUTime
import Text.Printf

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.MultiSet (MultiSet)
import Data.Maybe (fromJust)
import Data.List (foldl', isPrefixOf, sort, intercalate)
import Debug.Trace (trace)

processInput :: String -> Map String (Set String)
processInput = Map.fromListWith Set.union . map handleLine . concatMap addInv . lines
  where
    handleLine :: (String, String) -> (String, Set String)
    handleLine (s1, s2) = (s1, Set.singleton s2)

    addInv :: String -> [(String, String)]
    addInv s = [(pre, post), (post, pre)] where (pre, _:post) = break (== '-') s

exploreTriangles :: Map String (Set String) -> String -> Set (String, String, String)
exploreTriangles m s = Set.foldl' Set.union Set.empty
    . Set.map closeTriangle . Set.filter (> s) . fromJust . Map.lookup s $ m
  where
    closeTriangle :: String -> Set (String, String, String)
    closeTriangle e = Set.map (s,e,) . Set.filter (> e)
        $ Set.intersection
            (fromJust $ Map.lookup s m)
            (fromJust $ Map.lookup e m)

solveA :: Map String (Set String) -> Set (String, String, String)
solveA m = foldl' Set.union Set.empty . map (exploreTriangles m) . Map.keys $ m

expandClique :: Map String (Set String) -> (Set String, String) -> Set String
expandClique graph (clique, lastVal) 
    | Set.null candidates = clique
    | otherwise           = findLargestClique graph candidates
  where
    candidates = Set.map (\x -> (Set.insert x clique, x))
        . Set.filter (> lastVal)
        . Set.foldl' Set.intersection (Set.fromList . Map.keys $ graph)
        . Set.map (fromJust . flip Map.lookup graph) $ clique

findLargestClique :: Map String (Set String) -> Set (Set String, String) -> Set String
findLargestClique graph candidates = Set.foldl' maxBySize Set.empty 
    $ Set.map (expandClique graph) candidates
  where
    maxBySize a b = if Set.size a > Set.size b then a else b

solveB :: Map String (Set String) -> Set (Set String, String) -> String
solveB m = intercalate "," . sort . Set.toList . findLargestClique m

main :: IO ()
main = do
    let inputData        = "day-23-input.txt"
    let inputDataExample = "day-23-input-example.txt"

    startTotal <- getCPUTime
    input <- processInput <$> readFile inputData
    start <- getCPUTime
    let resA = solveA input
    putStrLn . (++) "Problem 1:  " . show . length . Set.filter (\(a, b, c) -> any (isPrefixOf "t") [a, b, c]) $ resA
    end <- getCPUTime
    printf "Time:       %.3fs\n\n" $ toSeconds start end

    start <- getCPUTime
    putStrLn . (++) "Problem 2:  " . solveB input
        $ Set.map (\(x, y, z) -> (Set.fromList [x, y, z], z)) resA
    end <- getCPUTime
    printf "Time:       %.3fs\n\n" $ toSeconds start end

    endTotal <- getCPUTime
    printf "Total Time: %.3fs\n" $ toSeconds startTotal endTotal
  where
    toSeconds :: Integer -> Integer -> Double
    toSeconds start end = fromIntegral (end - start) / (10^12)

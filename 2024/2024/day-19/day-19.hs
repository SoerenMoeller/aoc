import System.CPUTime
import Text.Printf

import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isNothing)
import Data.List (nub, isPrefixOf)
import Debug.Trace (trace)
import Data.Set (Set)
import qualified Data.MultiSet as MS
import qualified Data.Set as Set

inputData :: String
inputData = "day-19-input.txt"

inputDataExample :: String
inputDataExample = "day-19-input-example.txt"

processInput :: String -> (Trie, [String])
processInput xs = (trie, suffix)
  where
    ([fl],_:suffix) = break null . lines $ xs
    trie = buildTrie $ splitOn ", " fl

data Trie = Trie
    { isWordEnd :: Bool
    , children  :: Map Char Trie
    } deriving (Show)

emptyTrie :: Trie
emptyTrie = Trie False Map.empty

insert :: String -> Trie -> Trie
insert [] (Trie _ children) = Trie True children
insert (c:cs) (Trie isEnd children) = Trie isEnd (Map.insert c updatedSubTrie children)
  where
    subTrie = Map.findWithDefault emptyTrie c children
    updatedSubTrie = insert cs subTrie

allMatchingPrefixes :: Trie -> String -> [String]
allMatchingPrefixes = go ""
  where
    go :: String -> Trie -> String -> [String]
    go _ _ [] = []
    go prefix (Trie isEnd children) (c:cs) = case Map.lookup c children of
        Nothing -> []
        Just subTrie ->
          let newPrefix = prefix ++ [c]
              validPrefix = ([newPrefix | isWordEnd subTrie])
          in validPrefix ++ go newPrefix subTrie cs

buildTrie :: [String] -> Trie
buildTrie = foldr insert emptyTrie

constistsOfSubstrings :: Trie -> String -> Bool
constistsOfSubstrings _    ""    = True
constistsOfSubstrings trie xs = any (constistsOfSubstrings trie . (`drop` xs)) prefixLength
  where
    prefixLength = nub . map length . allMatchingPrefixes trie $ xs

solveA :: Trie -> [String] -> Int
solveA trie = length . filter (constistsOfSubstrings trie)

allCompositions' :: Trie -> Map String Int -> String -> (Int, Map String Int)
allCompositions' trie memo "" = (1, memo)
allCompositions' trie memo xs 
    | Just result <- Map.lookup xs memo = (result, memo)
    | otherwise = (results, finalMemo)
  where
    (results, updatedMemo) = foldl accumulate (0, memo) prefix
    finalMemo = Map.insert xs results updatedMemo
    prefix = allMatchingPrefixes trie xs
    
    accumulate (total, memoAcc) p = 
      let (res, newMemo) = allCompositions' trie memoAcc $ drop (length p) xs
      in (total + res, newMemo)

allCompositions :: Trie -> String -> Int
allCompositions trie xs = fst $ allCompositions' trie Map.empty xs

solveB :: Trie -> [String] -> Int
solveB trie = sum . map (allCompositions trie)

printTime :: Bool -> Integer -> Integer -> IO ()
printTime True start end  = printf "Total Time: %.3fs\n" (fromIntegral (end - start) / (10^12) ::  Double)
printTime False start end = printf "Time:       %.3fs\n\n" (fromIntegral (end - start) / (10^12) ::  Double)

main :: IO ()
main = do
    startTotal <- getCPUTime
    (trie, composed) <- processInput <$> readFile inputData
    start <- getCPUTime
    putStrLn . (++) "Problem 1:  " . show . solveA trie $ composed
    end <- getCPUTime
    printTime False start end

    start <- getCPUTime
    putStrLn . (++) "Problem 2:  " . show . solveB trie $ composed
    end <- getCPUTime
    printTime False start end

    endTotal <- getCPUTime
    printTime True startTotal endTotal

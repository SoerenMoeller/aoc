import System.CPUTime
import Text.Printf

import Data.Bits
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (zip4)
import Debug.Trace (trace)

processInput :: String -> [Integer]
processInput = map read . lines

nextSecret :: Integer -> Integer
nextSecret secret = step3
  where
    step1 = (secret `xor` (secret `shiftL` 6)) .&. 16777215
    step2 = step1 `xor`  (step1 `shiftR` 5)
    step3 = (step2 `xor`  (step2 `shiftL` 11)) .&. 16777215

solveA :: [Integer] -> Integer
solveA = sum . map ((!! 2000) . iterate nextSecret)

toMap :: [((Integer, Integer, Integer, Integer), Integer)] -> Map (Integer, Integer, Integer, Integer) Integer
toMap = toMap' Map.empty
  where
    toMap' :: Map (Integer, Integer, Integer, Integer) Integer
           -> [((Integer, Integer, Integer, Integer), Integer)]
           -> Map (Integer, Integer, Integer, Integer) Integer
    toMap' m [] = m
    toMap' m (x:xs) = case Map.lookup (fst x) m of
        Just v  -> toMap' m xs
        Nothing -> toMap' (uncurry Map.insert x m) xs

pack4 :: [Integer] -> Map (Integer, Integer, Integer, Integer) Integer
pack4 xs = toMap. map (\((d1,_), (d2,_), (d3,_), (d4,p)) -> ((d1, d2, d3, d4), p)) $ tuple4
  where
    tuple4 :: [((Integer, Integer), (Integer, Integer), (Integer, Integer), (Integer, Integer))]
    tuple4 = zip4 ys (tail ys) (tail . tail $ ys) (tail . tail . tail $ ys)
      where ys = priceAndDiff

    lastDigit :: [Integer]
    lastDigit = map (`mod` 10) xs

    priceAndDiff :: [(Integer, Integer)]
    priceAndDiff = zipWith (\pl pc -> (pc - pl, pc)) lastDigit (tail lastDigit)

solveB :: [Integer] -> Integer
solveB xs = maxPrice
  where
    results :: [Map (Integer, Integer, Integer, Integer) Integer]
    results = map (pack4 . take 2002 . iterate nextSecret) xs

    combindedMap :: Map (Integer, Integer, Integer, Integer) Integer
    combindedMap = foldr (Map.unionWith (+)) Map.empty results

    maxPrice :: Integer
    maxPrice = maximum . Map.elems $ combindedMap

prettyPrintMap :: Map (Integer, Integer, Integer, Integer) Integer -> IO ()
prettyPrintMap m = mapM_ printEntry (Map.toList m)
  where
    printEntry ((d1, d2, d3, d4), v) =
      putStrLn $ printf "Key: (%d, %d, %d, %d) | Value: %d" d1 d2 d3 d4 v

main :: IO ()
main = do
    let inputData        = "day-22-input.txt"
    let inputDataExample = "day-22-input-example.txt"

    startTotal <- getCPUTime
    input <- processInput <$> readFile inputData
    start <- getCPUTime
    putStrLn . (++) "Problem 1:  " . show . solveA $ input
    end <- getCPUTime
    printf "Time:       %.3fs\n\n" $ toSeconds start end

    start <- getCPUTime
    putStrLn . (++) "Problem 2:  " . show . solveB $ input
    end <- getCPUTime
    printf "Time:       %.3fs\n\n" $ toSeconds start end

    endTotal <- getCPUTime
    printf "Total Time: %.3fs\n" $ toSeconds startTotal endTotal
  where
    toSeconds :: Integer -> Integer -> Double
    toSeconds start end = fromIntegral (end - start) / (10^12)

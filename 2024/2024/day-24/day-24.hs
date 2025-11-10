import System.CPUTime
import Text.Printf

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Bits
import Data.Maybe (fromJust)
import Data.List (foldl', isPrefixOf)

type VarMap  = Map String Bool
type CalcMap = Map String (String, String, String)
type PreMap  = Map String (Set String)

op :: String -> (Bool -> Bool -> Bool)
op "XOR" = xor
op "AND" = (.&.)
op "OR"  = (.|.)

processInput :: String -> (VarMap, CalcMap)
processInput xs = (vars, calcs)
  where
    (top, _:bottom) = break null $ lines xs
    vars  = Map.fromList . map ((,) <$> take 3 <*> parseBool . drop 5) $ top
    calcs = Map.fromList . map ((\[v1, o, v2, _, res] -> (res, (v1, o, v2))) . words) $ bottom
    
    parseBool "1" = True
    parseBool "0" = False

evalVar :: VarMap -> CalcMap -> String -> (Bool, VarMap)
evalVar vars calcs var = case Map.lookup var vars of
    Just res -> (res, vars)
    Nothing  -> (res, vars'')
      where
        (var1, fn, var2) = fromJust $ Map.lookup var calcs
        ([res1, res2], vars') = foldl' g ([], vars) [var1, var2]
        res = op fn res1 res2
        vars'' = Map.insert var res  vars

        g :: ([Bool], VarMap) -> String -> ([Bool], VarMap)
        g (xs, m) v = (r:xs, m')
          where
            (r, m') = evalVar m calcs v

bti :: Bool -> Int
bti True  = 1
bti False = 0

evalZ :: VarMap -> CalcMap -> [Bool]
evalZ vars calcs = 
    reverse
    . fst
    . foldl' g ([], vars) 
    . filter (isPrefixOf "z") . Map.keys $ calcs
  where
    g :: ([Bool], VarMap) -> String -> ([Bool], VarMap)
    g (xs, m) v = (r:xs, m')
      where
        (r, m') = evalVar m calcs v

solveA :: VarMap -> CalcMap -> Int
solveA vars = sum . zipWith (\i val -> bti val * 2^i) [0..] . evalZ vars 

idxToStr :: Int -> String
idxToStr idx = if length res == 1 then '0':res else res
  where res = show idx

solveB :: VarMap -> CalcMap -> String
solveB vars calcs =  "digraph G {\n" 
             ++ Map.foldrWithKey varToStr "" vars ++ "\n"
             ++ concatMap (\i -> "    z" ++ idxToStr i ++ " [label=\"z" ++ idxToStr i ++ "\"];\n") [0..45] ++ "\n"
             ++ snd (Map.foldrWithKey gateToStr (1, "") calcs)
             ++ "}"
  where
    varToStr :: String -> Bool -> String -> String
    varToStr xs b old = "    " ++ xs ++ " [label=\"" ++ xs ++ ": " ++ show (bti b) ++ "\"];\n" ++ old

    gateToStr :: String -> (String, String, String) -> (Int, String) -> (Int, String)
    gateToStr out (in1, fn, in2) (idx, old) = (idx + 1, res ++ old)
      where 
        res =  "    gate" ++ show idx ++ " [label=\"" ++ fn ++ "\", shape=box];\n"
            ++ "    " ++ in1 ++ " -> " ++ "gate" ++ show idx ++ ";\n"
            ++ "    " ++ in2 ++ " -> " ++ "gate" ++ show idx ++ ";\n"
            ++ "    " ++ "gate" ++ show idx ++ " -> " ++ out ++ ";\n\n"

main :: IO ()
main = do
    let inputData        = "day-24-input.txt"
    let inputDataExample = "day-24-input-example.txt"

    startTotal <- getCPUTime
    (vars, calcs) <- processInput <$> readFile inputData
    start <- getCPUTime
    putStrLn . (++) "Problem 1:  " . show . solveA vars $ calcs
    end <- getCPUTime
    printf "Time:       %.3fs\n\n" $ toSeconds start end

    start <- getCPUTime
    let fileName = "part2.dot"
    let result   = solveB vars calcs
    putStrLn "Problem 2:  Writing to part2.dot, use graphviz."
    end <- getCPUTime
    writeFile fileName result
    putStrLn "Result after visual inspection: fkb,nnr,rdn,rqf,rrn,z16,z31,z37"
    printf "Time:       %.3fs\n\n" $ toSeconds start end
    
    endTotal <- getCPUTime
    printf "Total Time: %.3fs\n" $ toSeconds startTotal endTotal
  where
    toSeconds :: Integer -> Integer -> Double
    toSeconds start end = fromIntegral (end - start) / (10^12)

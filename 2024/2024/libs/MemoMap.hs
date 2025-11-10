module MemoMap where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (foldl')

addMemo :: Ord k => (k -> v) -> (k -> Map k v -> (v, Map k v))
addMemo f k m = case Map.lookup k m of
    Just v -> (v, m)
    Nothing -> (v, Map.insert k v m) where v = f k

threadMemo :: Ord k 
           => (k -> Map k v -> (v, Map k v)) 
           -> [k] 
           -> Map k v 
           -> ([v], Map k v)
threadMemo f ks m = foldl' g ([], m) ks 
  where
    g (vs, m) k = (vs ++ [v], m') where (v, m') = f k m

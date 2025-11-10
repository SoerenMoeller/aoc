module Search where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sequence (Seq( (:<|) ), (><))
import qualified Data.Sequence as Seq

dfs :: Ord a => (a -> [a]) -> (a -> Bool) -> a -> Maybe a
dfs succ goal start = go Set.empty $ Seq.singleton start
  where
    go _ Seq.Empty = Nothing
    go visited (x:<|xs)
      | goal x = Just x
      | Set.member x visited = go visited xs
      | otherwise = go (Set.insert x visited) $ Seq.fromList (succ x) >< xs

bfs :: Ord a => (a -> [a]) -> (a -> Bool) -> a -> Maybe a
bfs succ goal start = go Set.empty (Seq.singleton start)
  where
    go _ Seq.Empty = Nothing
    go visited (x :<| xs)
      | goal x = Just x
      | Set.member x visited = go visited xs
      | otherwise = go (Set.insert x visited) $ xs >< Seq.fromList (succ x)

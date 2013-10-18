module QC.HeapProps where

import Data.List (sort)
import Control.Monad (liftM)
import Test.QuickCheck

-- import QC.Heap 
import QC.SkewHeap

instance (Arbitrary a, Ord a) => Arbitrary (Heap a) where
  arbitrary = liftM heapify arbitrary

  shrink Nil = []
  shrink h = [delete h]

-- peek (insert empty x) == x
prop_single :: Int -> Bool
prop_single x = peek (insert empty x) == x

-- peek (heapify xs) == minimum xs
prop_heapify :: [Int] -> Property
prop_heapify xs = not (null xs) ==> peek (heapify xs) == minimum xs

-- peek (delete (heapify xs)) == head (tail (sort xs))
prop_delete :: [Int] -> Property
prop_delete xs =
  length xs >= 2 ==> peek (delete (heapify xs)) == head (tail (sort xs))

-- peek (foldl merge empty heaps) == minimum (map peek heaps)
prop_merge :: [Heap Int] -> Property
prop_merge heaps =
  not (null heaps) && all (not . isEmpty) heaps
  ==> peek (foldl merge empty heaps) == minimum (map peek heaps)

-- forall n . n <= left n && n <= right n
prop_heap :: Heap Int -> Bool
prop_heap Nil = True
prop_heap (Node Nil Nil _) = True
prop_heap (Node (Node _ _ lv) Nil v) = v <= lv
prop_heap (Node Nil (Node _ _ rv) v) = v <= rv
prop_heap (Node (Node _ _ lv) (Node _ _ rv) v) = v <= lv && v <= rv

tests :: [(String, Property)]
tests = [ ("peek (insert empty x) == x", property prop_single)
        , ("peek (heapify xs) == minimum xs", property prop_heapify)
        , ( "peek (delete (heapify xs)) == head (tail (sort xs))"
          , property prop_delete
          )
        , ( "peek (foldl merge empty heaps) == minimum (map peek heaps)"
          , property prop_merge
          )
        , ("forall n . n <= left n && n <= right n", property prop_heap)
        ]

main :: IO ()
main = mapM_ (\(s, p) -> do { putStrLn s; quickCheck p}) tests

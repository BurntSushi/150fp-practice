module QC.Heap
  ( Heap(..)
  , empty, isEmpty, insert, merge, peek, delete, heapify
  )
where

import Control.Monad (liftM)
import Data.List (sort)
import Test.QuickCheck
import Text.Printf (printf)

data Heap a = Nil
            | Node { left :: Heap a, right :: Heap a, value :: a }
            deriving Eq
            -- a min-heap

instance Show a => Show (Heap a) where
  show = show' 0
    where show' d Nil = printf "%sNil\n" $ indent d
          show' d (Node l r v) = this ++ show' (d+1) l ++ show' (d+1) r
            where this = printf "%sNode %s\n" (indent d) (show v)

          indent d = concat $ replicate d "  "

empty :: Ord a => Heap a
isEmpty :: Ord a => Heap a -> Bool
single :: Ord a => a -> Heap a
insert :: Ord a => Heap a -> a -> Heap a
merge :: Ord a => Heap a -> Heap a -> Heap a
peek :: Ord a => Heap a -> a
delete :: Ord a => Heap a -> Heap a
heapify :: Ord a => [a] -> Heap a

empty = Nil

isEmpty Nil = True
isEmpty _   = False

single = Node Nil Nil

insert Nil x = single x
insert n@(Node l r v) x =
  if x <= v then Node n Nil x else
    case (l, r) of
      (Nil, Nil) -> Node (single x) Nil v
      (Node{}, Nil) -> Node l (single x) v
      (Nil, Node{}) -> Node (single x) r v
      (Node _ _ lv, Node _ _ rv) ->
        if lv <= rv then Node (insert l x) r v else Node l (insert r x) v

merge h Nil = h
merge h (Node l r v) = insert (merge (merge h l) r) v

peek Nil = error "cannot peek empty heap"
peek (Node _ _ v) = v

delete Nil = Nil
delete (Node l r v) = merge l r

heapify = foldl insert empty


-- random testing

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

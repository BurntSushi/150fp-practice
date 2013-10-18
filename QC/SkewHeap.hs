module QC.SkewHeap
  ( Heap(..)
  , empty, isEmpty, insert, merge, peek, delete, heapify
  )
where

import Data.Monoid (Monoid, mappend, mempty)
import Text.Printf (printf)

-- | A skew min-heap.
data Heap a = Nil
            | Node { left :: Heap a, right :: Heap a, value :: a }
            deriving Eq

instance Ord a => Monoid (Heap a) where
  mempty = empty
  mappend = merge

instance Show a => Show (Heap a) where
  show = show' 0
    where show' d Nil = printf "%sNil\n" $ indent d
          show' d (Node l r v) = this ++ show' (d+1) l ++ show' (d+1) r
            where this = printf "%sNode %s\n" (indent d) (show v)

          indent d = concat $ replicate d "  "

empty :: Ord a => Heap a
empty = Nil

isEmpty :: Ord a => Heap a -> Bool
isEmpty Nil = True
isEmpty _   = False

single :: Ord a => a -> Heap a
single = Node Nil Nil

insert :: Ord a => Heap a -> a -> Heap a
insert h = merge h . single

peek :: Ord a => Heap a -> a
peek Nil = error "cannot peek empty heap"
peek (Node _ _ v) = v

delete :: Ord a => Heap a -> Heap a
delete Nil = Nil
delete (Node l r v) = merge l r

heapify :: Ord a => [a] -> Heap a
heapify = foldl insert empty

-- | Implements the alternating left/right subtree merge which makes
-- this a skew tree.
merge :: Ord a => Heap a -> Heap a -> Heap a
merge Nil h = h
merge h Nil = h
merge n1 n2 = Node (merge (right s) b) (left s) (value s)
  where (s, b) = if value n1 <= value n2 then (n1, n2) else (n2, n1)

module QC.Heap
  ( Heap(..)
  , empty, isEmpty, insert, merge, peek, delete, heapify
  )
where

import Data.Monoid (Monoid, mappend, mempty)
import Text.Printf (printf)

-- | A boring functional min-heap.
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
insert Nil x = single x
insert n@(Node l r v) x =
  if x <= v then Node n Nil x else
    case (l, r) of
      (Nil, Nil) -> Node (single x) Nil v
      (Node{}, Nil) -> Node l (single x) v
      (Nil, Node{}) -> Node (single x) r v
      (Node _ _ lv, Node _ _ rv) ->
        if lv <= rv then Node (insert l x) r v else Node l (insert r x) v

peek :: Ord a => Heap a -> a
peek Nil = error "cannot peek empty heap"
peek (Node _ _ v) = v

delete :: Ord a => Heap a -> Heap a
delete Nil = Nil
delete (Node l r v) = merge l r

heapify :: Ord a => [a] -> Heap a
heapify = foldl insert empty

merge :: Ord a => Heap a -> Heap a -> Heap a
merge h Nil = h
merge h (Node l r v) = insert (merge (merge h l) r) v

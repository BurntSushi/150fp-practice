{-# LANGUAGE MultiParamTypeClasses #-}

module QC.RBTree
where

-- much of this code adapted from Functional Pearls
-- Red-Black Trees in a Functional Setting

import Data.Monoid

data Color = Red
           | Black deriving (Eq)

data Ord a => RBTree a = Empty | Node Color (RBTree a) a (RBTree a) deriving
           (Eq, Show)

instance Show Color where
    show Red = "Red"
    show Black = "Black"

instance Ord a => Monoid (RBTree a) where
    mempty = Empty
    mappend = undefined

--this shit is weird
--instance Show a => Show (RBTree a) where
--    show (Node c l v r) = "(" ++ show c ++ show l ++ show v ++ show r ++ ")"
--    show Empty = "Empty"

member :: Ord a => a -> RBTree a -> Bool
member elem Empty = False
member elem (Node _ l x r) | elem < x  = (member elem l)
                           | elem > x  = (member elem r)
                           | elem == x = True

rbify :: Ord a => [a] -> RBTree a
rbify = foldr insert Empty

insert :: Ord a => a -> RBTree a -> RBTree a
insert elem tree = makeBlack (ins tree)
  where ins Empty = Node Red Empty elem Empty
        ins (Node color l val r) | elem < val  = balance color (ins l) val r
                                 | elem == val = Node color l val r
                                 | elem > val  = balance color l val (ins r)

makeBlack :: Ord a => RBTree a -> RBTree a
makeBlack (Node _ l a r) = Node Black l a r

--makeRed :: (Ord a) => RBTree a -> RBTree a
--makeRed (Node _ l a r) = Node Red l a r

balance :: Ord a => Color -> RBTree a -> a -> RBTree a -> RBTree a
balance Black (Node Red (Node Red a x b) y c) z d =
          Node Red (Node Black a x b) y (Node Black c z d)
balance Black (Node Red a x (Node Red b y c)) z d =
          Node Red (Node Black a x b) y (Node Black c z d)
balance Black a x (Node Red (Node Red b y c) z d) =
          Node Red (Node Black a x b) y (Node Black c z d)
balance Black a x (Node Red b y (Node Red c z d)) =
          Node Red (Node Black a x b) y (Node Black c z d)
balance color a x b = Node color a x b

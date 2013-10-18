{-# LANGUAGE FlexibleInstances #-}
module Main where

import Control.Monad (unless)
import Data.List (nub)
import Data.Monoid (Monoid, mappend, mempty)
import System.Environment (getArgs)
import System.Random (getStdGen)

import qualified Monads.MRand as R
import qualified Monads.Dot as Dot

import qualified QC.SkewHeap as SH
import qualified QC.Heap as H

import qualified QC.RBTree as RB

-- | General functions to traverse any heap.
class Heapable h where
  -- | Returns the "left" node. The left node of a nil node is nil.
  left :: Ord a => h a -> h a

  -- | Returns the "right" node. The right node of a nil node is nil.
  right :: Ord a => h a -> h a

  -- | True if the heap is empty.
  isEmpty :: Ord a => h a -> Bool

  -- | Returns the value at the given node.
  -- Behavior is undefined if the given heap is empty.
  value :: Ord a => h a -> a

instance Heapable RB.RBTree where
    left RB.Empty = RB.Empty
    left (RB.Node _ l x r) = l

    right RB.Empty = RB.Empty
    right (RB.Node _ l x r) = r

    isEmpty RB.Empty = True
    isEmpty _ = False

    value RB.Empty = error "this doesn't make sense"
    value (RB.Node _ _ x _) = x

instance Heapable SH.Heap where
  left SH.Nil = SH.Nil
  left n = SH.left n

  right SH.Nil = SH.Nil
  right n = SH.right n

  isEmpty = SH.isEmpty

  value SH.Nil = error "no value for Nil nodes"
  value (SH.Node _ _ v) = v

instance Heapable H.Heap where
  left H.Nil = H.Nil
  left n = H.left n

  right H.Nil = H.Nil
  right n = H.right n

  isEmpty = H.isEmpty

  value H.Nil = error "no value for Nil nodes"
  value (H.Node _ _ v) = v

instance (Heapable h, Show a, Ord a) => Dot.Node (h a) where
  name = show . value
  label _ = Nothing

-- | Graphs any value satisfying `Heapable`.
heapVis :: (Monoid (h a), Heapable h, Dot.Node (h a), Show a, Ord a) =>
           h a ->
           Dot.Dotter (h a) ()
heapVis = vis mempty
  where vis p node =
          unless (isEmpty node) $ do
            unless (isEmpty p) (Dot.addEdge p node)
            Dot.addNode node
            vis node (left node)
            vis node (right node)

-- | Translates any value satisfying `Heapable` to its Dot representation.
dotit :: (Monoid (h a), Heapable h, Show a, Ord a) => h a -> String
dotit = Dot.eval . heapVis

-- | Picks a heap based on command line arguments and translates it to
-- its Dot representation using a random list of integers as labels.
pickHeap :: [String] -- ^ Command line arguments.
            -> [Int] -- ^ Random list of integers.
            -> String -- ^ Dot code.
pickHeap [] = dotit . SH.heapify
pickHeap ["skew"] = dotit . SH.heapify
pickHeap ["heap"] = dotit . H.heapify
pickHeap ["rb"] = dotit . RB.rbify
pickHeap bunk = error ("unrecognized heap " ++ concat bunk)

-- Outputs a random dot graph using the heap specified.
main :: IO ()
main = do
  args <- getArgs
  g <- getStdGen
  let xs = R.eval (R.randomRs (1, 100000000)) g
  putStrLn $ pickHeap args $ take 100 $ nub xs


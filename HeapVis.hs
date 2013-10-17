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

-- | 
class Heapable h where
  left :: Ord a => h a -> h a
  right :: Ord a => h a -> h a
  isEmpty :: Ord a => h a -> Bool
  value :: Ord a => h a -> a -- runtime error for nil nodes

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

heapVis :: (Monoid (h a), Heapable h, Show a, Ord a) =>
           h a ->
           Dot.Dotter Dot.SimpleNode ()
heapVis = vis mempty
  where vis p hnode =
          if isEmpty hnode then return () else do
            let gnode = asGraphNode hnode
            unless (isEmpty p) (Dot.addEdge (asGraphNode p) gnode)
            Dot.addNode gnode
            vis hnode (left hnode)
            vis hnode (right hnode)
  
        asGraphNode = Dot.simpleNode . show . value

dotit :: (Monoid (h a), Heapable h, Show a, Ord a) => h a -> String
dotit = Dot.eval . heapVis

pickHeap :: [String] -> [Int] -> String
pickHeap [] = dotit . SH.heapify
pickHeap ["skew"] = dotit . SH.heapify
pickHeap ["heap"] = dotit . H.heapify
pickHeap bunk = error ("unrecognized heap " ++ concat bunk)

main :: IO ()
main = do
  args <- getArgs
  g <- getStdGen
  let xs = R.eval (R.randomRs (1, 100000000)) g
  putStrLn $ pickHeap args $ take 100 $ nub xs


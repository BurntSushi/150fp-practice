module HeapVis where

import Control.Monad (unless)
import Data.List (nub)
import System.Random (getStdGen)

import qualified Monads.MRand as R
import qualified Monads.Dot as Dot
import qualified QC.SkewHeap as H

heapVis :: (Show a, Ord a) => H.Heap a -> Dot.Dotter Dot.SimpleNode ()
heapVis = heapVis' H.Nil
  where heapVis' _ H.Nil = return ()
        heapVis' p node@(H.Node l r v) = do
          let this = Dot.simpleNode $ show v
          unless (H.isEmpty p) (Dot.addEdge (asSimple p) this)
          Dot.addNode this
          heapVis' node l
          heapVis' node r

        asSimple H.Nil = error "cannot make Dot node from Nil heap node"
        asSimple (H.Node _ _ v) = Dot.simpleNode $ show v

main :: IO ()
main = do
  g <- getStdGen
  let xs = R.eval (R.randomRs (1, 100000000)) g :: [Int]
  let heap = H.heapify $ take 100 $ nub xs
  putStrLn $ Dot.eval (heapVis heap)
  print heap


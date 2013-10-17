module Monads.Dot
  ( Node, SimpleNode, Dotter
  , simpleNode, simpleNodeLabel
  , addNode, addEdge, addEdgeLabel
  , eval
  )
where

import Data.List (intercalate)
import Data.Monoid (Monoid, mappend, mempty)
import Text.Printf (printf)

-- | @Node@ corresponds to any value representable as a Dot node.
class Node a where
  name :: a -> String
  label :: a -> Maybe String -- labels may not contain double quotes

-- | Every edge is a directed edge from one node to another.
data Edge a = Edge { from :: a, to :: a, elabel :: Maybe String }

-- | @SimpleNode@ is a convenience implementation satisfying @Node@.
data SimpleNode = SimpleNode { sname :: String, slabel :: Maybe String }

instance Node SimpleNode where
  name = sname
  label = slabel


data Graph a = Graph { nodes :: [a], edges :: [Edge a] }

dot :: Node a => Graph a -> String
dot (Graph ns es) = printf "digraph G {\n\t%s\n\t%s\n}" nodes edges
  where nodes = intercalate "\n\t" $ map dnode ns
        edges = intercalate "\n\t" $ map dedge es
        mkLabel = printf " [label=\"%s\"] "

        dnode n = printf "%s%s;" (name n) lbl
          where lbl = maybe "" mkLabel (label n)
        dedge e = printf "%s -> %s%s" (name $ from e) (name $ to e) lbl
          where lbl = maybe "" mkLabel (elabel e)

instance Monoid (Graph a) where
  mempty = Graph [] []
  (Graph ns1 es1) `mappend` (Graph ns2 es2) = Graph (ns1 ++ ns2) (es1 ++ es2)

newtype Dotter n a = Dotter { runDotter :: (a, Graph n) }

instance Monad (Dotter n) where
  return a = Dotter (a, mempty)
  Dotter (a, g) >>= f = Dotter (a', g `mappend` g')
    where (a', g') = runDotter (f a)

simpleNode :: String -> SimpleNode
simpleNode name = SimpleNode name Nothing

simpleNodeLabel :: String -> String -> SimpleNode
simpleNodeLabel name label = SimpleNode name (Just label)

addNode :: Node a => a -> Dotter a ()
addNode node = Dotter ((), Graph [node] [])

addEdge :: Node a => a -> a -> Dotter a ()
addEdge n1 n2 = Dotter ((), Graph [] [Edge n1 n2 Nothing])

addEdgeLabel :: Node a => a -> a -> String -> Dotter a ()
addEdgeLabel n1 n2 label = Dotter ((), Graph [] [Edge n1 n2 (Just label)])

eval :: Node a => Dotter a () -> String
eval = dot . snd . runDotter

testGraph :: Dotter SimpleNode ()
testGraph = do
  let (n1, n2) = (simpleNode "A", simpleNode "B")
  addNode n1
  addNode n2
  addEdge n1 n2

main :: IO ()
main = putStrLn $ eval testGraph

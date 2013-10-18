module Monads.Dot
  ( Node(..)
  , SimpleNode, Dotter
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
  -- | A uniquely identifying string for this node.
  name :: a -> String

  -- | Labels cannot contain double quotes. If no label is specified,
  -- then `name` will be used instead.
  label :: a -> Maybe String

-- | Every edge is a directed edge from one node to another.
data Edge a = Edge { from :: a, to :: a, elabel :: Maybe String }

-- | @SimpleNode@ is a convenience implementation satisfying @Node@.
data SimpleNode = SimpleNode { sname :: String, slabel :: Maybe String }

instance Node SimpleNode where
  name = sname
  label = slabel

-- | Create a new @SimpleNode@ with a name.
simpleNode :: String -- ^ The name of the node.
              -> SimpleNode
simpleNode name = SimpleNode name Nothing

-- | Same as `simpleNode`, but allows a label independent of the name.
simpleNodeLabel :: String -- ^ The name of the node.
                   -> String -- ^ The label of the node.
                   -> SimpleNode
simpleNodeLabel name label = SimpleNode name (Just label)

-- | @Graph@ corresponds to a graph expressible in the Dot language.
data Graph a = Graph { nodes :: [a], edges :: [Edge a] }

-- | Converts a graph to its Dot representation.
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

-- | A monad that constructs values representable in the Dot language.
newtype Dotter n a = Dotter { runDotter :: (a, Graph n) }

instance Monad (Dotter n) where
  return a = Dotter (a, mempty)
  Dotter (a, g) >>= f = Dotter (a', g `mappend` g')
    where (a', g') = runDotter (f a)

-- | Adds a node to the current graph.
addNode :: Node a => a -> Dotter a ()
addNode node = Dotter ((), Graph [node] [])

-- | Adds a directed edge to the current graph.
addEdge :: Node a
           => a -- ^ The "from" node.
           -> a -- ^ The "to" node.
           -> Dotter a ()
addEdge n1 n2 = Dotter ((), Graph [] [Edge n1 n2 Nothing])

-- | Same as `addEdge`, but with an edge label.
addEdgeLabel :: Node a
                => a -- ^ The "from" node.
                -> a -- ^ The "to" node.
                -> String -- ^ The edge label.
                -> Dotter a ()
addEdgeLabel n1 n2 label = Dotter ((), Graph [] [Edge n1 n2 (Just label)])

-- | Evaluate an instance of the `Dotter` monad to its Dot representation.
eval :: Node a => Dotter a () -> String
eval = dot . snd . runDotter

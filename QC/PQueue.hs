module QC.PQueue where

import Control.Monad (liftM2)
import Test.QuickCheck (Arbitrary, arbitrary, shrink)
import Text.Printf (printf)

import qualified QC.Heap as H

data Clocked a = Clocked { ticks :: Int, value :: a }

data Q a = Q { elems :: H.Heap (Clocked a), clock :: Int }

instance Show a => Show (Q a) where
  show (Q heap clock) = printf "Clock: %d\n%s" clock (show heap)

instance Show a => Show (Clocked a) where
  show (Clocked t v) = printf "%s (ticks %d)" (show v) t

instance Eq a => Eq (Clocked a) where
  (==) (Clocked t1 v1) (Clocked t2 v2) = (t1, v1) == (t2, v2)

instance Ord a => Ord (Clocked a) where
  compare c1 c2 = compare (ticks c1, value c1) (ticks c2, value c2)

empty :: Ord a => Q a
isEmpty :: Ord a => Q a -> Bool
put  :: Ord a => Q a -> a -> Q a
get  :: Ord a => Q a -> a  -- defined on nonempty queue only
rest :: Ord a => Q a -> Q a -- defined on nonempty queue only

empty = Q H.empty 0
isEmpty = H.isEmpty . elems
put (Q heap clock) x = Q (H.insert heap (Clocked clock x)) (clock + 1)
get = value . H.peek . elems
rest (Q heap clock) = Q (H.delete heap) clock

-- instance (Arbitrary a, Ord a) => Arbitrary (Q a) where 
  -- arbitrary = liftM2 Q arbitrary arbitrary 
--  
-- instance Arbitrary a => Arbitrary (Clocked a) where 
  -- arbitrary = liftM2 Clocked arbitrary arbitrary 


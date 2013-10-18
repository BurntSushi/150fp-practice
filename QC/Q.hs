module QC.Q
  (Q, empty, isEmpty, put, get, rest)
where

import Control.Monad (liftM2)
import Test.QuickCheck
import Text.Printf (printf)
  
-- | A queue with O(1) `put` and amortized O(1) `get` operations.
data Q a = Q { front :: [a], back :: [a] }
           -- corresponds to front ++ reverse back

instance Show a => Show (Q a) where
  show (Q xs ys) = printf "(F: %s, B: %s)" (show xs) (show ys)

empty :: Q a
empty = Q [] []

isEmpty :: Q a -> Bool
isEmpty q = null (front q) && null (back q)

-- | Adds an element to the back of the queue.
put  :: Q a -> a -> Q a
put q a = Q (front q) (a : back q)
-- put q a = Q [] (a : back q) -- nr's error #1

-- | Returns an element from the top of the queue.
-- Calling @get@ on an empty queue results in a runtime error.
get  :: Q a -> a  -- defined on nonempty queue only
get (Q [] [])    = error "get from empty queue"
get (Q [] as)    = get (Q (reverse as) [])
get (Q (a:as) _) = a

-- | Returns the given queue with the top of the queue removed.
-- Calling @rest@ on an empty queue results in a runtime error.
rest :: Q a -> Q a -- defined on nonempty queue only
rest (Q [] [])    = error "rest from empty queue"
rest (Q [] as)    = rest (Q (reverse as) [])
rest (Q (a:as) bs) = Q as bs
-- rest (Q (a:as) _) = Q as [] -- nr's error #2


-- random testing

-- | QTerms represents a series of *valid* combinations of queue constructors.
data QTerms = Empty
            | Rest QTerms
            | Put QTerms
            deriving Show

-- | Enumerates all valid queues up to a certain number of constructor
-- applications.
enumerate :: Int -- ^ Maximum number of queue terms to enumerate.
             -> [QTerms] -- ^ All valid permutations of queue constructors.
enumerate = e [] Empty 0
  where e :: [QTerms] -- ^ Accumulated permutations.
             -> QTerms -- ^ Initial set of queue terms.
             -> Int -- ^ Number of items in the queue.
             -> Int -- ^ Number of queue terms left to generate.
             -> [QTerms]
        e perms _ _ 0 = perms
        e perms ts 0 n = e (next:perms) next 1 (n-1)
          where next = Put ts
        e perms ts i n = e (next:perms) next (i-1) (n-1)
                         ++ e (next':perms) next' (i+1) (n-1)
          where (next, next') = (Rest ts, Put ts)

instance Arbitrary QTerms where
  arbitrary = elements $ enumerate 15

  -- Removing the outer most constructor application always results
  -- in a valid queue.
  shrink Empty = []
  shrink (Rest ts) = [ts]
  shrink (Put ts) = [ts]

-- | Constructs a queue given a list of values and a construction of
-- a queue via `QTerms`.
asQ :: [a] -> QTerms -> Q a
asQ _ Empty = empty
asQ as (Rest ts) = rest $ asQ as ts
asQ (a:as) (Put ts) = put (asQ as ts) a

-- | Convenience function for building a queue of integers.
intQ :: QTerms -> Q Int
intQ = asQ [1..]

-- | Returns the last item in a queue.
lastItem :: Ord a => Q a -> a
lastItem q = if isEmpty (rest q) then get q else lastItem (rest q)

-- | Drains all of the items in the queue and returns the empty queue.
drain :: Ord a => Q a -> Q a
drain q
  | isEmpty q = empty
  | otherwise = drain (rest q)

-- get (put empty x) == x
prop_empty :: Int -> Bool
prop_empty x = get (put empty x) == x

-- isEmpty (rest (put empty x)) == true
prop_rest :: Int -> Bool
prop_rest x = isEmpty (rest (put empty x))

-- lastItem (put queue x) == x
prop_last :: QTerms -> Int -> Bool
prop_last ts x = lastItem (put (intQ ts) x) == x

-- isEmpty (drain q) == true
prop_drainable :: QTerms -> Bool
prop_drainable ts = isEmpty (drain (intQ ts))

tests :: [(String, Property)]
tests = [ ("get (put empty x) == x", property prop_empty)
        , ("isEmpty (rest (put empty x)) == true", property prop_rest)
        , ("lastItem (put queue x) == x", property prop_last)
        , ("isEmpty (drain q) == true", property prop_drainable)
        ]

main :: IO ()
main = mapM_ (\(s, p) -> do { putStrLn s; quickCheck p}) tests

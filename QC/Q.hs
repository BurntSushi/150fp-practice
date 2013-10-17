module QC.Q
  (Q, empty, isEmpty, put, get, rest)
where

import Control.Monad (liftM2)
import Test.QuickCheck
import Text.Printf (printf)
  
data Q a = Q { front :: [a], back :: [a] }
           -- corresponds to front ++ reverse back

instance Show a => Show (Q a) where
  show (Q xs ys) = printf "(F: %s, B: %s)" (show xs) (show ys)

empty :: Q a
isEmpty :: Q a -> Bool
put  :: Q a -> a -> Q a
get  :: Q a -> a  -- defined on nonempty queue only
rest :: Q a -> Q a -- defined on nonempty queue only

empty = Q [] []

isEmpty q = null (front q) && null (back q)

-- put q a = Q [] (a : back q) 
put q a = Q (front q) (a : back q)

get (Q [] [])    = error "get from empty queue"
get (Q [] as)    = get (Q (reverse as) [])
get (Q (a:as) _) = a

rest (Q [] [])    = error "rest from empty queue"
rest (Q [] as)    = rest (Q (reverse as) [])
rest (Q (a:as) bs) = Q as bs
-- rest (Q (a:as) _) = Q as [] 


-- random testing

data QTerms = Empty
            | Rest QTerms
            | Put QTerms
            deriving Show

-- max # of queue terms -> all valid combos 
enumerate :: Int -> [QTerms]
enumerate = e [] Empty 0
  where e :: [QTerms] -> QTerms -> Int -> Int -> [QTerms]
        e perms _ _ 0 = perms
        e perms ts 0 n = e (next:perms) next 1 (n-1)
          where next = Put ts
        e perms ts i n = e (next:perms) next (i-1) (n-1)
                         ++ e (next':perms) next' (i+1) (n-1)
          where (next, next') = (Rest ts, Put ts)

instance Arbitrary QTerms where
  arbitrary = elements $ enumerate 15

  shrink Empty = []
  shrink (Rest ts) = [ts]
  shrink (Put ts) = [ts]

asQ :: [a] -> QTerms -> Q a
asQ _ Empty = empty
asQ as (Rest ts) = rest $ asQ as ts
asQ (a:as) (Put ts) = put (asQ as ts) a

intQ :: QTerms -> Q Int
intQ = asQ [1..]

lastItem :: Ord a => Q a -> a
lastItem q = if isEmpty (rest q) then get q else lastItem (rest q)

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

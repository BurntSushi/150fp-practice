module QC.QueueProps where

import Test.QuickCheck (Property, property, quickCheck)

-- import Q 
import QC.PQueue

lastItem :: Ord a => Q a -> a
lastItem q = if isEmpty (rest q) then get q else lastItem (rest q)

drain :: Ord a => Q a -> Q a
drain q
  | isEmpty q = empty
  | otherwise = drain (rest q)

-- get (put empty x) == x
prop_empty :: Int -> Bool
prop_empty x = get (put empty x) == x

-- lastItem (put queue x) == x
-- This is not true for priority queues!
prop_last :: Q Int -> Int -> Bool
prop_last q x = lastItem (put q x) == x

-- isEmpty (drain q) == true
prop_drainable :: Q Int -> Bool
prop_drainable q = isEmpty (drain q)

-- get (rest (put (put empty x) y)) == y
prop_puts :: (Int, Int) -> Bool
prop_puts (x, y) = get (rest (put (put empty x) y)) == y

-- get (put (rest (put (put empty x) y)) z) == y
prop_restPuts :: (Int, Int, Int) -> Bool
prop_restPuts (x, y, z) = get (put (rest (put (put empty x) y)) z) == y

tests :: [(String, Property)]
tests = [ ("empty", property prop_empty)
        , ("last", property prop_last)
        , ("drainable", property prop_drainable)
        , ("puts", property prop_puts)
        , ("restPuts", property prop_restPuts)
        ]

main :: IO ()
main = mapM_ (\(s, p) -> do { putStrLn s; quickCheck p}) tests


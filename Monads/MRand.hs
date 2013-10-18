module Monads.MRand
  ( MRand
  , eval, random, randomR, randoms, randomRs
  )
where

import qualified System.Random as R

-- | A random monad that works with any generator satisfying `RandomGen`.
newtype MRand g a = MRand (g -> (a, g))

instance R.RandomGen g => Monad (MRand g) where
  return a = MRand (\g -> (a, g))
  MRand m0 >>= f = MRand (\g -> let (a, g') = m0 g
                                    MRand m1 = f a
                                 in m1 g')

-- | Evaluate an instance of the `MRand` monad with a source of randomness.
eval :: R.RandomGen g => MRand g a -> g -> a
eval (MRand f) g = fst (f g)

-- | Provides a random value.
random :: (R.RandomGen g, R.Random a) => MRand g a
random = MRand R.random

-- | Provides a random value in a range.
randomR :: (R.RandomGen g, R.Random a)
           => (a, a) -- ^ An inclusive range [lo, hi].
           -> MRand g a
randomR r = MRand (R.randomR r)

-- | Provides an infinite list of random values.
randoms :: (R.RandomGen g, R.Random a) => MRand g [a]
randoms = MRand (\g -> let (g', g'') = R.split g in (R.randoms g', g''))

-- | Provides an infinite list of random values in a range.
randomRs :: (R.RandomGen g, R.Random a)
            => (a, a) -- ^ An inclusive range [lo, hi].
            -> MRand g [a]
randomRs r = MRand (\g -> let (g', g'') = R.split g in (R.randomRs r g', g''))

-- Demonstrates two infinite, independent sources of integers.
two :: R.RandomGen g => MRand g ([Int], [Int])
two = do
  xs <- randomRs (1, 10)
  ys <- randomRs (1, 10)
  return (xs, ys)

main = do
  g <- R.getStdGen
  let (xs, ys) = eval two g
  print $ take 10 xs
  print $ take 10 ys

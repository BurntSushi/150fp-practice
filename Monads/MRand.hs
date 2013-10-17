module Monads.MRand
  ( MRand
  , eval, random, randomR, randoms, randomRs
  )
where

import qualified System.Random as R

newtype MRand g a = MRand (g -> (a, g))

instance R.RandomGen g => Monad (MRand g) where
  return a = MRand (\g -> (a, g))
  MRand m0 >>= f = MRand (\g -> let (a, g') = m0 g
                                    MRand m1 = f a
                                 in m1 g')

eval :: R.RandomGen g => MRand g a -> g -> a
eval (MRand f) g = fst (f g)

random :: (R.RandomGen g, R.Random a) => MRand g a
random = MRand R.random

randomR :: (R.RandomGen g, R.Random a) => (a, a) -> MRand g a
randomR r = MRand (R.randomR r)

randoms :: (R.RandomGen g, R.Random a) => MRand g [a]
randoms = MRand (\g -> let (g', g'') = R.split g in (R.randoms g', g''))

randomRs :: (R.RandomGen g, R.Random a) => (a, a) -> MRand g [a]
randomRs r = MRand (\g -> let (g', g'') = R.split g in (R.randomRs r g', g''))

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


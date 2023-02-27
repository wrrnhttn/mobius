-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- NOTE: quickly writing out the obvious implementations, some of this is quite inefficient!
-- TODO: many of these functions can be generalized, we simply need to take instance of e.g. Integral
--       to get appropriate numeric operations

module Mobius () where

import Data.Bifunctor (bimap)

-- import Data.PosInt

-- newtype Complex = Complex (Double, Double) deriving (Num) 

-- some arithmetic functions
zeta :: Int -> Int
zeta n = 1

idf :: Int -> Int
idf = id

coprimes :: Int -> [Int]
coprimes n = filter (\y -> gcd n y == 1) [1 .. n]

-- euler phi function (maybe rename eulerPhi? there are many 'phi's...)
phi :: Int -> Int
phi x = length $ coprimes x

-- identity for dirichlet convolution
epsilon :: Int -> Int
epsilon x = if x == 1 then 1 else 0

-- posify :: (PosInt -> a) -> Int -> Maybe a
-- posify f x = case toPosInt x of
--   Nothing -> Nothing
--   Just n -> Just $ f n

divides :: Int -> Int -> Bool
divides x y = mod y x == 0

divisors :: Int -> [Int]
divisors n = filter (`divides` n) [1..n]

d :: Int -> Int
d = length . divisors

sigma :: Int -> Int
sigma = sum . divisors

divisorPairs :: Int -> [(Int, Int)]
divisorPairs n = zip (divisors n) (reverse $ divisors n)

-- NOTE: can use unzipWith
opDivisorPairs :: (Int -> Int -> Int) -> Int -> [Int]
opDivisorPairs f n = fmap (uncurry f) (divisorPairs n)

convolveBy :: (Int -> Int -> Int) -> (Int -> Int) -> (Int -> Int) -> (Int -> Int)
convolveBy op f g = \n -> sum $ fmap (\x -> uncurry op x) (fgDivs n)
  where
    fgDivs n = map (bimap f g) (divisorPairs n)

convolution = convolveBy (+)

dirichletConvolution = convolveBy (*)
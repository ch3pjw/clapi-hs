module Arbitrary where

import Test.QuickCheck

import Control.Monad (replicateM)


boundedListOf :: Int -> Int -> Gen a -> Gen [a]
boundedListOf lo hi g = choose (lo, hi) >>= flip replicateM g

smallListOf :: Gen a -> Gen [a]
smallListOf = boundedListOf 0 5

smallListOf1 :: Gen a -> Gen [a]
smallListOf1 = boundedListOf 1 5

maybeOf :: Gen a -> Gen (Maybe a)
maybeOf g = oneof [return Nothing, Just <$> g]

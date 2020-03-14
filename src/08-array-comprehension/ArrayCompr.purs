-- Based on chapter 4.11 of the PureScript Book
module ArrayCompr where

import Prelude

import Control.MonadZero (guard)
import Data.Array (length, (..))
import Data.Int (pow)

factors :: Int -> Array (Array Int)
factors n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  pure [i, j]

isPrime :: Int -> Boolean
isPrime n = (length $ factors n) == 1

cartesianProduct :: Array Int -> Array Int -> Array (Array Int)
cartesianProduct xs ys = do
  x <- xs
  y <- ys
  pure [x, y]

triples :: Int -> Array (Array Int)
triples n = do
  a <- 1 .. n
  b <- a .. n
  c <- b .. n
  guard $ a `pow` 2 + b `pow` 2 == c `pow` 2
  pure [a, b, c]
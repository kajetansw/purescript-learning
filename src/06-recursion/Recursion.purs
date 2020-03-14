module Recursion (fact, fib, isEven) where 

import Prelude

import Data.Maybe (Maybe(..))
  
fact :: Int -> Maybe Int
fact 0 = Just 1
fact n = 
  if n > 0
    then Just (n * unsafeFact (n - 1)) 
    else Nothing
  where 
    unsafeFact :: Int -> Int
    unsafeFact 0 = 1
    unsafeFact x = x * unsafeFact (x - 1)

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

isEven :: Int -> Boolean
isEven 0 = true
isEven n = not isEven (n - 1)

module PatMat where
  
import Prelude

import Data.Maybe (Maybe(..))

fact :: Int -> Int
fact 0 = 0
fact 1 = 1
fact n = n * fact (n - 1)

binCoeff :: Int -> Int -> Int
binCoeff n 0 = 1
binCoeff n k 
  | n == k = 1
  | n > k && k > 0 = binCoeff (n - 1) (k -1) + binCoeff (n - 1) k
binCoeff _ _ = 0

sameCity :: { city :: String } -> { city :: String } -> Boolean
sameCity p1 p2 = p1.city == p2.city

fromSingleton :: forall a. a -> Array a -> a
fromSingleton default [x] = x
fromSingleton default _ = default 

-- chapter 5.12
data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String

data Point = Point
  { x :: Number
  , y :: Number
  }

origin :: Point
origin = Point { x, y }
  where
    x = 0.0
    y = 0.0

defaultCircle :: Shape
defaultCircle = Circle origin 10.0

getText :: Shape -> Maybe String
getText (Text _ t) = Just t
getText _ = Nothing

sortPair :: Array Int -> Array Int
sortPair arr@[x, y]
  | x <= y = arr
  | otherwise = [y, x]
sortPair arr = arr

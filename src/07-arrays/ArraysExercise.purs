module ArraysExercise where
  
import Prelude

import Data.Array (filter)
import Data.Int (pow)

square :: Array Int -> Array Int
square = map (\n -> n `pow` 2)

filterPositives :: Array Int -> Array Int
filterPositives = filter (\n -> n >= 0)

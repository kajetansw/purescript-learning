module Utils where
  
import Data.Number.Format (toStringWith, fixed)

toStringFixedToOne :: Number -> String
toStringFixedToOne = toStringWith (fixed 1)
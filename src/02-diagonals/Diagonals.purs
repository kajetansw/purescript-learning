module Diagonals where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Math (sqrt)
import Utils (toStringFixedToOne)

diagonal :: Number -> Number -> Number
diagonal w h = sqrt (w * w + h * h)

main :: Effect Unit
main = do 
    log ((diagonal 3.0 4.0) # toStringFixedToOne)
    -- log (toStringFixedToOne (diagonal 3.0 4.0))
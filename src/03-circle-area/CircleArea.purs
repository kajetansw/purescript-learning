module CircleArea where
  
import Prelude

import Effect (Effect)
import Effect.Console (log)
import Math (pi, pow)
import Utils (toStringFixedToOne)

circleArea :: Number -> Number
circleArea r = pi * (pow r 2.0)

main :: Effect Unit
main = do
    log ((circleArea 2.0) # toStringFixedToOne)
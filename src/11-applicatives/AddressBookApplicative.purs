module AddressBookApplicative where
  
import Prelude

import Control.Apply (lift2)
import Data.Maybe (Maybe)
import Data.String as S
import Data.Validation.Semigroup (V, invalid)

newtype Address = Address
  { street :: String
  , city   :: String
  , state  :: String
  }

instance showAddress :: Show Address where
  show (Address o) = "Address " <>
    "{ street: " <> show o.street <>
    ", city: "   <> show o.city <>
    ", state: "  <> show o.state <>
    " }"

address :: String -> String -> String -> Address
address street city state = Address { street, city, state }

-- chapter 7.8

addMaybe :: Maybe Number -> Maybe Number -> Maybe Number
addMaybe x y = lift2 (+) x y
-- addMaybe x y = (+) <$> x <*> y

-- chapter 7.9

type Errors = Array String

nonEmpty :: String -> String -> V Errors Unit
nonEmpty field "" = invalid ["Field '" <> field <> "' cannot be empty"]
nonEmpty _     _  = pure unit

lengthIs :: String -> Int -> String -> V Errors Unit
lengthIs field len value | S.length value /= len =
  invalid ["Field '" <> field <> "' must have length " <> show len]
lengthIs _     _   _     =
  pure unit

validateAddress :: Address -> V Errors Address
validateAddress (Address o) =
  address <$> (nonEmpty "Street" o.street *> pure o.street)
          <*> (nonEmpty "City"   o.city   *> pure o.city)
          <*> (lengthIs "State" 2 o.state *> pure o.state)

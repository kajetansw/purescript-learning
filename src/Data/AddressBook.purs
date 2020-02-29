module Data.AddressBook where
  
import Prelude

import Control.Plus (empty)

import Data.List (List(..), filter, head, null, nubBy)
import Data.Maybe (Maybe)

type Entry =
    { firstName :: String
    , lastName :: String
    , address :: Address
    }

type Address =
    { street :: String
    , city :: String
    , state :: String
    }

type AddressBook = List Entry

showEntry :: Entry -> String
showEntry entry = 
    entry.lastName <> ", "
    <> entry.firstName <> ": "
    <> showAddress entry.address


showAddress :: Address -> String
showAddress addr =
    addr.street <> ", "
    <> addr.city <> ", "
    <> addr.state

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName = head <<< filter filterEntry
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.firstName == firstName && entry.lastName == lastName  

printEntry :: String -> String -> AddressBook -> Maybe String
printEntry firstName lastName book = map showEntry (findEntry firstName lastName book)

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street = head <<< filter filterEntry
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.address.street == street  

addressBookContainsName :: String -> String -> AddressBook -> Boolean
addressBookContainsName firstName lastName book = not null $ filter filterEntry book
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

equalsByName :: Entry -> Entry -> Boolean
equalsByName e1 e2 = e1.firstName == e2.firstName && e1.lastName == e2.lastName  

removeDuplicatesByName :: AddressBook -> AddressBook
removeDuplicatesByName = nubBy equalsByName


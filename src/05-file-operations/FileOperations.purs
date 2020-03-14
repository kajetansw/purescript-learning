module FileOperations where

import Prelude

import Data.Array (concatMap, filter, foldl, (:))
import Data.Maybe (Maybe(..))
import Data.Path (Path, isDirectory, ls, size)

allFiles :: Path -> Array Path
allFiles root = root : concatMap allFiles (ls root)

allFiles' :: Path -> Array Path
allFiles' file = file : do
  child <- ls file
  allFiles' child

onlyFiles :: Path -> Array Path
onlyFiles = (filter isFile) <<< allFiles
  where
    isFile :: Path -> Boolean
    isFile = not <<< isDirectory

findLargestFile :: Path -> Maybe Path
findLargestFile = ((foldl largestFile) Nothing) <<< onlyFiles
  where
    largestFile :: Maybe Path -> Path -> Maybe Path
    largestFile (Just x) y = 
      if (size x) > (size y) then Just x else Just y
    largestFile Nothing y = 
      Just y
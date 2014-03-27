module Control.GroupWith where
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.GroupWiths
-- Copyright   :  (c) Uli Köhler 2014
-- License     :  Apache License v2.0
-- Maintainer  :  ukoehler@techoverflow.net
-- Stability   :  provisional
-- Portability :  portable
--
-- A collection of grouping utility functions.
-- For a given function that assigns a key to objects,
-- provides functions that group said objects into a multimap
-- by said key.
--
-- This can be used similarly to the SQL GROUP BY statement.
--
-- Provides a more flexible approach to GHC.Exts.groupWith
--
-- > groupWith (take 1) ["a","ab","bc"]
--     == Map.fromList [("a",["a","ab"]), ("b",["bc"])]
--  
-----------------------------------------------------------------------------

import Data.Map (Map)
import qualified Data.Map as Map

type MultiMap a b = Map a [b]

-- | Group values in a list by a key, generated
--   by a given function. The resulting map contains
--   for each generated key the values (from the given list)
--   that yielded said key by applying the function on said value
groupWith :: (Ord b) => (a -> b) -> [a] -> MultiMap b a
groupWith f xs = Map.fromListWith (++) [(f x, [x]) | x <- xs]

-- | Like groupBy, but the identifier-generating function
--   may generate multiple keys for each value (or none at all).
--   The corresponding value from the original list will be placed
--   in the identifier-corresponding map entry for each generated
--   identifier
groupWithMultiple :: (Ord b) => (a -> [b]) -> [a] -> MultiMap b a
groupWithMultiple f xs = 
  let identifiers x = [(val, [x]) | val <- f x]
  in Map.fromListWith (++) $ concat [identifiers x | x <- xs]

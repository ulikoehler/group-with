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
--   that yielded said key by applying the function on said value.
groupWith :: (Ord b) =>
             (a -> b) -- ^ The function used to map a list value to its key
          -> [a] -- ^ The list to be grouped
          -> MultiMap b a -- ^ The resulting key --> value multimap
groupWith f xs = Map.fromListWith (++) [(f x, [x]) | x <- xs]

-- | Like groupWith, but the identifier-generating function
--   may generate multiple keys for each value (or none at all).
--   The corresponding value from the original list will be placed
--   in the identifier-corresponding map entry for each generated
--   identifier.
--   Note that values are added to the 
groupWithMultiple :: (Ord b) =>
                     (a -> [b]) -- ^ The function used to map a list value to its keys
                  -> [a] -- ^ The list to be grouped
                  -> MultiMap b a -- ^ The resulting map
groupWithMultiple f xs = 
  let identifiers x = [(val, [x]) | val <- f x]
  in Map.fromListWith (++) $ concat [identifiers x | x <- xs]

-- | Like groupWith, but uses a custom combinator function
--   > groupWith == groupWithUsing (\v -> [v])
groupWithUsing :: (Ord b) =>
             (a -> c) -- ^ Transformer function used to map a value to the resulting type
          -> (c -> c -> c) -- ^ The combinator used to combine an existing value
                           --   for a given key with a new value
          -> (a -> b) -- ^ The function used to map a list value to its key
          -> [a] -- ^ The list to be grouped
          -> Map b c -- ^ The resulting key --> transformed value map
groupWithUsing t c f xs = Map.fromListWith c $ map (\v -> (f v, t v)) xs
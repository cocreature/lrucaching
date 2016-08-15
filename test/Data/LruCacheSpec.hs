module Data.LruCacheSpec 
  (spec
  ) where

import Data.Hashable
import Data.Maybe
import Prelude hiding (lookup)
import Test.Hspec
import Test.QuickCheck

import Data.LruCache
import Data.LruCache.SpecHelper

spec :: Spec
spec = 
  do describe "insertView" $ do
       it "evicts elements that were previously there" $ do 
         property (evictExisted :: LruCache SmallInt Int -> SmallInt -> Int -> Bool)
       it "removes evicted elements" $ do
         property (evictRemoved :: LruCache SmallInt Int -> SmallInt -> Int -> Bool)
     describe "insert" $ do
       it "inserts elements" $ do
         property (insertExists :: LruCache SmallInt Int -> SmallInt -> Int -> Property)


insertExists :: 
  (Hashable k, Ord k, Eq v, Show v) =>
  LruCache k v ->
  k ->
  v ->
  Property
insertExists cache k v =
  let cache' = insert k v cache
  in fmap fst (lookup k cache') === Just v

evictExisted :: (Hashable k, Ord k, Eq v) => LruCache k v -> k -> v -> Bool
evictExisted cache k v =
  let evicted = fst (insertView k v cache)
  in case evicted of
       Nothing       -> True
       Just (k', v') -> 
         case lookup k' cache of
           Nothing       -> False
           Just (v'', _) -> v' == v''

evictRemoved :: (Hashable k, Ord k) => LruCache k v -> k -> v -> Bool
evictRemoved cache k v =
  let (evicted, cache') = insertView k v cache
  in case evicted of
       Nothing     -> True
       Just (k',_) -> isNothing (lookup k' cache') 
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-|
Module      : Data.LruCache
Copyright   : (c) Moritz Kiefer, 2016
              (c) Jasper Van der Jeugt, 2015
License     : BSD3
Maintainer  : moritz.kiefer@purelyfunctional.org
Pure API to an LRU cache.
-}
module Data.LruCache
  ( LruCache
  , empty
  , insert
  , insertView
  , deleteView
  , lookup
  ) where

import qualified Data.HashPSQ as HashPSQ
import           Data.Hashable (Hashable)
import           Data.List.Compat (sortOn)
import           Data.Maybe (isNothing)
import           Prelude hiding (lookup)

import           Data.LruCache.Internal

-- | Create an empty 'LruCache' of the given size.
empty :: Int -> LruCache k v
empty capacity
  | capacity < 1 = error "LruCache.empty: capacity < 1"
  | otherwise    =
      LruCache 
        { lruCapacity = capacity
        , lruSize     = 0
        , lruTick     = 0
        , lruQueue    = HashPSQ.empty
        }

-- | Restore 'LruCache' invariants returning the evicted element if any.
trim' :: (Hashable k, Ord k) => LruCache k v -> (Maybe (k, v), LruCache k v)
trim' c
  | lruTick c == maxBound     =
      -- It is not physically possible to have that many elements but
      -- the clock could potentially get here. We then simply decrease
      -- all priorities in O(nlogn) and start over.
      let queue' = HashPSQ.fromList . compress . HashPSQ.toList $ lruQueue c
      in trim' $!
           c { lruTick = fromIntegral (lruSize c)
             , lruQueue = queue'
             }
  | lruSize c > lruCapacity c = 
      let Just (k, _, v) = HashPSQ.findMin (lruQueue c)
          c' = c  { lruSize  = lruSize c - 1
                  , lruQueue = HashPSQ.deleteMin (lruQueue c)
                  }
      in seq c' (Just (k, v), c')
  | otherwise                 = (Nothing, c)

compress :: [(k,Priority,v)] -> [(k,Priority,v)]
compress q =
  let sortedQ = sortOn (\(_,p,_) -> p) q
  in zipWith (\(k,_,v) p -> (k,p,v)) sortedQ [1..]

-- TODO benchmark to see if this is actually faster than snd . trim'
-- | Restore 'LruCache' invariants. For performance reasons this is
-- not @snd . trim'@.
trim :: (Hashable k, Ord k) => LruCache k v -> LruCache k v
trim c
  | lruTick c == maxBound     = empty (lruCapacity c)
  | lruSize c > lruCapacity c = 
      c  { lruSize  = lruSize c - 1
         , lruQueue = HashPSQ.deleteMin (lruQueue c)
         }
  | otherwise                 = c

-- | Insert an element into the 'LruCache'.
insert :: (Hashable k, Ord k) => k -> v -> LruCache k v -> LruCache k v
insert key val c =
  trim $!
  let (mbOldVal,queue) = HashPSQ.insertView key (lruTick c) val (lruQueue c)
  in c  { lruSize  = if isNothing mbOldVal
                     then lruSize c + 1
                     else lruSize c
        , lruTick  = lruTick c + 1
        , lruQueue = queue
        }

-- | Delete an element from the 'LruCache'.
deleteView :: (Hashable k, Ord k) => k -> LruCache k v -> (Maybe v, LruCache k v)
deleteView key c = 
  case HashPSQ.deleteView key (lruQueue c) of
    Nothing                 -> (Nothing, c)
    Just (p,mbOldVal,queue) -> ( Just mbOldVal
                               , c  { lruSize  = lruSize c - 1
                                    , lruQueue = queue
                                    }
                               )

-- | Insert an element into the 'LruCache' returning the evicted
-- element if any.
--
-- When the logical clock reaches its maximum value and all values are
-- evicted 'Nothing' is returned.
insertView :: (Hashable k, Ord k) => k -> v -> LruCache k v -> (Maybe (k, v), LruCache k v)
insertView key val cache =
  let (mbOldVal,queue) = 
        HashPSQ.insertView key (lruTick cache) val (lruQueue cache)
  in trim' $! cache 
       { lruSize  = if isNothing mbOldVal
                    then lruSize cache + 1
                    else lruSize cache
       , lruTick  = lruTick cache + 1
       , lruQueue = queue
       }

-- | Lookup an element in an 'LruCache' and mark it as the least
-- recently accessed.
lookup :: (Hashable k, Ord k) => k -> LruCache k v -> Maybe (v, LruCache k v)
lookup k c = 
  case HashPSQ.alter lookupAndBump k (lruQueue c) of
    (Nothing, _) -> Nothing
    (Just x, q)  ->
      let !c' = trim $ c {lruTick = lruTick c + 1, lruQueue = q}
      in Just (x, c')
  where
    lookupAndBump Nothing       = (Nothing, Nothing)
    lookupAndBump (Just (_, x)) = (Just x,  Just ((lruTick c), x))

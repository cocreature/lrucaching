{-# LANGUAGE BangPatterns #-}
{-|
Module      : Data.LruCache.IO.Finalizer
Copyright   : (c) Moritz Kiefer, 2016
              (c) Jasper Van der Jeugt, 2015
License     : BSD3
Maintainer  : moritz.kiefer@purelyfunctional.org
Convenience module for the common case of caching results of IO actions 
when finalizers have to be run when cache entries are evicted.
-}
module Data.LruCache.IO.Finalizer
  ( LruHandle(..)
  , newLruHandle
  , cached
  , StripedLruHandle(..)
  , newStripedLruHandle
  , stripedCached
  ) where

import           Control.Applicative ((<$>))
import           Data.Foldable (traverse_)
import           Data.Hashable (Hashable, hash)
import           Data.IORef (IORef, atomicModifyIORef', newIORef)
import           Data.Tuple (swap)
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           Prelude hiding (lookup)

import           Data.LruCache

-- | Store a LRU cache in an 'IORef to be able to conveniently update it.
newtype LruHandle k v = LruHandle (IORef (LruCache k (v, v -> IO ())))

-- | Create a new LRU cache of the given size.
newLruHandle :: Int -> IO (LruHandle k v)
newLruHandle capacity = LruHandle <$> newIORef (empty capacity)

-- | Return the cached result of the action or, in the case of a cache
-- miss, execute the action and insert it in the cache.
cached ::
  (Hashable k, Ord k) =>
  LruHandle k v ->
  k ->
  IO v ->
  (v -> IO ()) {- ^ finalizer -} ->
  IO v
cached (LruHandle ref) k io finalizer =
  do lookupRes <- atomicModifyIORef' ref $ \c ->
       case lookup k c of
         Nothing      -> (c,  Nothing)
         Just (v, c') -> (c', Just v)
     case lookupRes of
       Just (!v,_)  -> return v
       Nothing      ->
         do v <- io
            evicted <- atomicModifyIORef' ref $ \c -> 
              swap (insertView k (v,finalizer) c)
            traverse_ (\(_,(v',finalize')) -> finalize' v') evicted
            return v

-- | Using a stripe of multiple handles can improve the performance in
-- the case of concurrent accesses since several handles can be
-- accessed in parallel.
newtype StripedLruHandle k v = StripedLruHandle (Vector (LruHandle k v))

-- | Create a new 'StripedHandle' with the given number of stripes and
-- the given capacity for each stripe.
newStripedLruHandle :: Int -> Int -> IO (StripedLruHandle k v)
newStripedLruHandle numStripes capacityPerStripe =
  StripedLruHandle <$> Vector.replicateM numStripes (newLruHandle capacityPerStripe)

-- | Striped version of 'cached'.
stripedCached ::
  (Hashable k, Ord k) =>
  StripedLruHandle k v ->
  k ->
  IO v ->
  (v -> IO ()) {- ^ finalizer -} ->
  IO v
stripedCached (StripedLruHandle v) k =
    cached (v Vector.! idx) k
  where
    idx = hash k `mod` Vector.length v
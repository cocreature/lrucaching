{-|
Module      : Data.LruCache.IO
Copyright   : (c) Moritz Kiefer, 2016
              (c) Jasper Van der Jeugt, 2015
License     : BSD3
Maintainer  : moritz.kiefer@purelyfunctional.org
Convenience module for the common case of caching results of IO actions.
See 'Data.LruCache.IO.Finalizer' if you want to run finalizers
automatically when cache entries are evicted
-}
module Data.LruCache.IO
  ( LruHandle(..)
  , cached
  , cachedMaybe
  , newLruHandle
  , StripedLruHandle(..)
  , stripedCached
  , stripedCachedMaybe
  , newStripedLruHandle
  , deleteViewIO
  , stripedDeleteViewIO
  ) where

import           Control.Applicative ((<$>))
import           Data.Hashable (Hashable, hash)
import           Data.IORef (IORef, atomicModifyIORef', newIORef)
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           Prelude hiding (lookup)

import           Data.LruCache

-- | Store a LRU cache in an 'IORef to be able to conveniently update it.
newtype LruHandle k v = LruHandle (IORef (LruCache k v))

-- | Create a new LRU cache of the given size.
newLruHandle :: Int -> IO (LruHandle k v)
newLruHandle capacity = LruHandle <$> newIORef (empty capacity)

-- | Return the cached result of the action or, in the case of a cache
-- miss, execute the action and insert it in the cache.
cached :: (Hashable k, Ord k) => LruHandle k v -> k -> IO v -> IO v
cached (LruHandle ref) k io =
  do lookupRes <- atomicModifyIORef' ref $ \c ->
       case lookup k c of
         Nothing      -> (c,  Nothing)
         Just (v, c') -> (c', Just v)
     case lookupRes of
       Just v  -> return v
       Nothing ->
         do v <- io
            atomicModifyIORef' ref $ \c -> (insert k v c, ())
            return v

-- | Maybe form of `cached`
cachedMaybe :: (Hashable k, Ord k) => LruHandle k v -> k -> IO (Maybe v) -> IO (Maybe v)
cachedMaybe (LruHandle ref) k io =
  do lookupRes <- atomicModifyIORef' ref $ \c ->
       case lookup k c of
         Nothing      -> (c,  Nothing)
         Just (v, c') -> (c', Just v)
     case lookupRes of
       Just v  -> return $ Just v
       Nothing ->
         do v <- io
            case v of
              Nothing -> return Nothing
              Just v' -> do
                atomicModifyIORef' ref $ \c -> (insert k v' c, ())
                return v

-- | Delete an item from the cache and return value of that item.
deleteViewIO :: (Hashable k, Ord k) => LruHandle k v -> k -> IO (Maybe v)
deleteViewIO (LruHandle ref) k =
  atomicModifyIORef' ref $ \c ->
    case deleteView k c of
      (Nothing, c') -> (c', Nothing)
      (Just v,  c') -> (c', Just v)

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
  IO v
stripedCached (StripedLruHandle v) k =
    cached (v Vector.! idx) k
  where
    idx = hash k `mod` Vector.length v

-- | Maybe form of `stripedCached`
stripedCachedMaybe ::
  (Hashable k, Ord k) =>
  StripedLruHandle k v ->
  k ->
  IO (Maybe v) ->
  IO (Maybe v)
stripedCachedMaybe (StripedLruHandle v) k =
    cachedMaybe (v Vector.! idx) k
  where
    idx = hash k `mod` Vector.length v


      
-- | Striped version of `deleteViewIO`
stripedDeleteViewIO ::
  (Hashable k, Ord k) =>
  StripedLruHandle k v ->
  k ->
  IO (Maybe v)
stripedDeleteViewIO (StripedLruHandle v) k =
    deleteViewIO (v Vector.! idx) k
  where
    idx = hash k `mod` Vector.length v

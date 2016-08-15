{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-|
Module      : Data.LruCache.Internal
Copyright   : (c) Moritz Kiefer, 2016
              (c) Jasper Van der Jeugt, 2015
License     : BSD3
Maintainer  : moritz.kiefer@purelyfunctional.org

This module contains internal datastructures. 
No guarantees are made as to the stability of this module
and violating invariants can result in unspecified behavior.
-}
module Data.LruCache.Internal
  ( LruCache(..)
  , Priority
  ) where
  
import           Control.DeepSeq (NFData,rnf)
import           Data.Int
import qualified Data.HashPSQ as HashPSQ

-- | Logical time at which an element was last accessed.
type Priority = Int64

-- | LRU cache based on hashing. The times of access are stored in a
-- monotonically increasing 'Int64', when that time is at 'maxbound'
-- the cache is emptied.
data LruCache k v = LruCache
  { lruCapacity :: !Int                         -- ^ The maximum number of elements in the queue
  , lruSize :: !Int                             -- ^ The current number of elements in the queue
  , lruTick :: !Priority                        -- ^ The next logical time
  , lruQueue :: !(HashPSQ.HashPSQ k Priority v) -- ^ Underlying priority queue
  } 
  deriving (Eq,Show,Functor,Foldable,Traversable)

instance (NFData k, NFData v) => NFData (LruCache k v) where
  rnf (LruCache cap size tick queue) =
    rnf cap `seq` rnf size `seq` rnf tick `seq` rnf queue

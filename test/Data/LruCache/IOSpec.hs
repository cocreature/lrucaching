module Data.LruCache.IOSpec
  (spec
  ) where

import           Control.Monad (foldM_)
import           Control.Monad.IO.Class
import           Data.IORef
import           Data.Set (Set)
import qualified Data.Set as Set
import           Test.Hspec
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Monadic as QC

import           Data.LruCache.IO
import           Data.LruCache.SpecHelper

spec :: Spec
spec =
  do describe "cached" $ do
       it "evicts leasts recently used elements" $ do
         QC.property historic

-- | Tests if elements not evicted have been recently accessed.
historic ::
  SmallInt ->             -- ^ Capacity
  [(SmallInt, String)] -> -- ^ Key-value pairs
  QC.Property             -- ^ Property
historic (SmallInt capacity) pairs = QC.monadicIO $
  do h <- liftIO $ newLruHandle capacity
     foldM_ (step h) [] pairs
     where
       step h history (k, v) = do
         wasInCacheRef <- liftIO $ newIORef True
         _             <- liftIO $ cached h k $ 
           do writeIORef wasInCacheRef False
              return v
         wasInCache    <- liftIO $ readIORef wasInCacheRef
         let recentKeys = nMostRecentKeys capacity Set.empty history
         QC.assert (Set.member k recentKeys == wasInCache)
         return ((k, v) : history)

nMostRecentKeys :: Ord k => Int -> Set k -> [(k, v)] -> Set k
nMostRecentKeys _ keys []   = keys
nMostRecentKeys n keys ((k, _) : history)
  | Set.size keys >= n      = keys
  | otherwise               =
      nMostRecentKeys n (Set.insert k keys) history

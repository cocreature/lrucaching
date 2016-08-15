{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module      : Data.LruCache.SpecHelper
Copyright   : (c) Moritz Kiefer, 2016
              (c) Jasper Van der Jeugt, 2015
License     : BSD3
Maintainer  : moritz.kiefer@purelyfunctional.org
-}
module Data.LruCache.SpecHelper where

import           Control.Applicative ((<$>),(<*>))
import           Data.Foldable (foldl')
import           Data.Hashable
import           Prelude hiding (lookup)
import qualified Test.QuickCheck as QC

import           Data.LruCache

data CacheAction k v
  = InsertAction k v
  | LookupAction k
  deriving (Show,Eq,Ord)

instance (QC.Arbitrary k, QC.Arbitrary v) => 
          QC.Arbitrary (CacheAction k v) where
  arbitrary = QC.oneof
    [ InsertAction <$> QC.arbitrary <*> QC.arbitrary
    , LookupAction <$> QC.arbitrary
    ]

applyCacheAction :: 
  (Hashable k, Ord k) =>
  CacheAction k v ->
  LruCache k v ->
  LruCache k v
applyCacheAction (InsertAction k v) c = 
  insert k v c
applyCacheAction (LookupAction k)   c = 
  case lookup k c of
    Nothing      -> c
    Just (_, c') -> c'

instance forall k v. 
         (QC.Arbitrary k, QC.Arbitrary v, Hashable k, Ord k) =>
         QC.Arbitrary (LruCache k v) where
  arbitrary = do
    capacity <- QC.choose (1, 50)
    (actions :: [CacheAction k v])  <- QC.arbitrary
    let !cache = empty capacity
    return $! foldl' (\c a -> applyCacheAction a c) cache actions

newtype SmallInt = SmallInt Int
  deriving (Eq, Ord, Show)

instance QC.Arbitrary SmallInt where
  arbitrary = SmallInt <$> QC.choose (1, 100)

instance Hashable SmallInt where
  hashWithSalt salt (SmallInt x) = (salt + x) `mod` 10

module Main where

import Test.Hspec
import qualified Data.LruCacheSpec as LruCacheSpec
import qualified Data.LruCache.IOSpec as IOSpec

main :: IO ()
main =
  hspec $ do
    describe "Data.LruCache" LruCacheSpec.spec
    describe "Data.LruCache.IO" IOSpec.spec

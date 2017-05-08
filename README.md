# lrucaching

[![Build Status](https://travis-ci.org/cocreature/lrucaching.svg?branch=master)](https://travis-ci.org/cocreature/lrucaching)
[![Hackage](https://img.shields.io/hackage/v/lrucaching.svg)](https://hackage.haskell.org/package/lrucaching)

An implementation of lrucaches based on a
[blogpost](https://jaspervdj.be/posts/2015-02-24-lru-cache.html) by
Jasper Van der Jeugt.

This package has no relation to
[lrucache](https://hackage.haskell.org/package/lrucache). I created it
because there were bugs in `lrucache` and the maintainer was not
responding to issues.


## Usage

The easiest way to use this library is to use `Data.LruCache.IO`. This wraps the
cache in a `Data.IORef`, a mutable varible in the `IO` monad.

e.g. To create a `1000`-item cache, keyed by `Integer`, storing `String`:

```haskell
import qualified Data.LruCache.IO as LRU

newCache :: IO (LRU.LruHandle Integer String)
newCache = LRU.newLruHandle 1000

cachedLookup cache key = LRU.cached cache key $
    -- insert some something expensive
    return $ show key

main :: IO ()
main = do
    cache <- newCache
    cachedLookup cache 123 >>= putStrLn
```

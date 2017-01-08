0.3.1
-----
* Allow vector == 0.12.*
* Do not use hspec-discover. This allows building the tests using
  `cabal new-build`.

0.3.0
-----
* Add `Data.LruCache.IO.Finalizer` for automatically running finalizer
  when evicting cache entrvies.
* Rename `newStripedHandle` to `newStripedLruHandle`

0.2.1
-----
* Fix build with GHC 7.8

0.2.0
----
* Don’t clear cache on clock overflow. This means that elements are
  never evicted without notifying the user via insertView.

0.1.0
-----
Initial release.

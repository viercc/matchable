# 0.2

- Add new derivation syntax taken from "recursion-schemes" package
  (see [Data.Functor.Foldable.TH](https://hackage.haskell.org/package/recursion-schemes-5.2.2.2/docs/Data-Functor-Foldable-TH.html))

# 0.1.2.1

- Require cabal-version: 2.0
- The library itself is unchanged. Update of the test code only.

# 0.1.2.0

- Fail correctly, with appropriate error message, when deriving Matchable
  and Bimatchable fails due to the datatype having zero (Matchable) or
  less than two (Bimatchable) parameters.

# 0.1.1.1

- Use "th-abstraction" >= 0.4 to cover changes in forthcoming
  template-haskell-2.17 bundled in GHC 9.0

# 0.1.1.0

- Depends on "th-abstraction" >= 0.3, rather than < 0.3 at
  previous version.

# 0.1.0.0

- Initial release.

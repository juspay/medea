# Dev

# 1.3.0

- Adds GHC 9.2.8 support
- Fix build errors with GHC 9.2.8
- Remove version bounds on dependency packages in Cabal file
- Adds new "cabal.project" file to project
- Handle Aeson V2.x `KeyMap` <-> `HashMap` data-type changes
- Handle Aeson V2.x `Key` <-> `Text` key-type changes
- Fix broken test-suite after project GHC upgrade
- Test with GHC 9.2.8
- Update `stack.yaml` to use LTS 20.26 (ghc 9.2.8 package-set)

# 1.2.0

- Widen QuickCheck bounds.
- Remove MonadError from the top-level API.
- Use strict, rather than lazy, bytestrings for parser input. This fixes issues
  with resource safety.
- Test with GHC 8.8.4.
- Change `stack.yaml` to use LTS 15.15.

# 1.1.2

- Ship our .hspec file to ensure all tests pass from an sdist.

# 1.1.1

- Ship our conformance suite as part of the sdist.

# 1.1.0

- Widen bounds on ``parser-combinators``.
- Export ``ParserError``, and have the loader return it on parsing errors.
- Remove -O2 optimization flag for test-suite.

# 1.0.0

- Initial release

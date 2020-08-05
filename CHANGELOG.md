# Dev

# 1.2.0

- Widen QuickCheck bounds.
- Remove MonadError from the top-level API.
- Use strict, rather than lazy, bytestrings for parser input. This fixes issues
  with resource safety.
- Test with GHC 8.8.4 (including CI).
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

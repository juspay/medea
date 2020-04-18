{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (lookup)
import Control.Monad.Except (runExcept)
import Data.Aeson (Value, encode, Value(..))
import Data.Aeson.Arbitrary
  ( ObjGenOpts(..),
    isArray,
    isBool,
    isNull,
    isNumber,
    isObject,
    isString,
    arbitraryObj,
    arbitraryValue
  )
import Data.Either (isLeft, isRight)
import Data.Medea (Schema, loadSchemaFromFile, validate)
import Data.HashMap.Strict (lookup)
import Data.Text (Text)
import Test.Hspec (Spec, describe, hspec, it, runIO, shouldNotSatisfy)
import Test.Hspec.Core.Spec (SpecM)
import Test.QuickCheck ((==>), forAll, property, Gen, Property)
import TestM (isParseError, isSchemaError, runTestM)

main :: IO ()
main = hspec $ do
  describe "Any schema" . testAny $ "any.medea"
  describe "Null schema" . testSingular "null.medea" "null" $ isNull
  describe "Boolean schema" . testSingular "boolean.medea" "boolean" $ isBool
  describe "Number schema" . testSingular "number.medea" "number" $ isNumber
  describe "String schema" . testSingular "string.medea" "string" $ isString
  describe "Array schema" . testSingular "array.medea" "array" $ isArray
  describe "Object schema" . testSingular "object.medea" "object" $ isObject
  describe "Boolean/null schema" . testSingular "nullable-boolean.medea" "boolean/null" $ isNull .|| isBool
  describe "Number/null schema" . testSingular "nullable-number.medea" "number/null" $ isNull .|| isNumber
  describe "String/null schema" . testSingular "nullable-string.medea" "string/null" $ isNull .|| isString
  describe "Array/null schema" . testSingular "nullable-array.medea" "array/null" $ isNull .|| isArray
  describe "Object/null schema" . testSingular "nullable-object.medea" "object/null" $ isNull .|| isObject
  -- Tests for object property checks.
  describe "Object schema with 1 property"
    . testObject (arbitraryObj $ ObjGenOpts ["foo"] [] 0 0) "1-property-no-additional-1.medea"
    $ hasProperty "foo" isBool
  describe "Object schema with 1 property"
    . testObject (arbitraryObj $ ObjGenOpts ["foo"] [] 0 0) "1-property-no-additional-2.medea"
    $ hasProperty "foo" isNull
  describe "Object schema with 1 property"
    . testObject (arbitraryObj $ ObjGenOpts [] ["foo"] 0 0) "1-property-no-additional-3.medea"
    $ hasOptionalProperty "foo" isArray
  describe "Object schema with 1 property and additional allowed"
    . testObject (arbitraryObj $ ObjGenOpts ["foo"] [] 0 3) "1-property-additional-1.medea"
    $ hasProperty "foo" isString
  describe "Object schema with 1 property and additional allowed"
    . testObject (arbitraryObj $ ObjGenOpts ["foo"] [] 0 3) "1-property-additional-2.medea"
    $ hasProperty "foo" isNumber
  describe "Object schema with 1 property and additional allowed"
    . testObject (arbitraryObj $ ObjGenOpts ["foo"] [] 0 3) "1-property-additional-3.medea"
    $ hasOptionalProperty "foo" isObject
  describe "Object schema with 3 properties"
    . testObject (arbitraryObj $ ObjGenOpts ["foo", "bar", "bazz"] [] 0 0) "3-property-no-additional-1.medea"
    $ hasProperty "foo" isBool .&& hasProperty "bazz" isString
  describe "Object schema with 3 properties"
    . testObject (arbitraryObj $ ObjGenOpts ["bar", "bazz"] ["foo"] 0 0) "3-property-no-additional-2.medea"
    $ hasOptionalProperty "foo" isNumber .&& hasProperty "bazz" isNull
  describe "Object schema with 3 properties and additional allowed"
    . testObject (arbitraryObj $ ObjGenOpts ["foo", "bar", "bazz"] [] 0 3) "3-property-additional-allowed-1.medea"
    $ hasProperty "foo" isBool .&& hasProperty "bazz" isString
  describe "Object schema with 3 properties and additional allowed"
    . testObject (arbitraryObj $ ObjGenOpts ["bar", "bazz"] ["foo"] 0 3) "3-property-additional-allowed-2.medea"
    $ hasOptionalProperty "foo" isNumber .&& hasProperty "bazz" isNull
  -- These tests are for objects where additional are not allowed but are still found.
  -- The generator is such that additional properties always exist.
  describe "Object schema with 1 property and no additional allowed"
    $ testInvalidObject (arbitraryObj $ ObjGenOpts ["foo"] [] 1 3) "1-property-no-additional-1.medea"
  describe "Object schema with 1 property and no additional allowed"
    $ testInvalidObject (arbitraryObj $ ObjGenOpts ["foo"] [] 1 3) "1-property-no-additional-2.medea"
  describe "Object schema with 1 property and no additional allowed"
    $ testInvalidObject (arbitraryObj $ ObjGenOpts [] ["foo"] 1 3) "1-property-no-additional-3.medea"
  describe "Object schema with 3 properties and no additional allowed"
    $ testInvalidObject (arbitraryObj $ ObjGenOpts ["foo", "bar", "bazz"] [] 1 3) "3-property-no-additional-1.medea"
  describe "Object schema with 3 properties and no additional allowed"
    $ testInvalidObject (arbitraryObj $ ObjGenOpts ["bar", "bazz"] ["foo"] 1 3) "3-property-no-additional-2.medea"

-- Helpers

(.||) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
f .|| g = (||) <$> f <*> g

(.&&) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
f .&& g = (&&) <$> f <*> g

testAny :: FilePath -> Spec
testAny fp = do
  scm <- loadAndParse $ prependTestDir fp
  it ("Should validate anything: " ++ fp) (yesProp arbitraryValue (const True) scm)

testSingular :: FilePath -> String -> (Value -> Bool) -> Spec
testSingular fp name p = do
  scm <- loadAndParse $ prependTestDir fp
  it ("Should validate " ++ name ++ "s: " ++ fp) (yesProp arbitraryValue p scm)
  it ("Should not validate non-" ++ name ++ "s: " ++ fp) (noProp arbitraryValue (not . p) scm)

testObject :: Gen Value -> FilePath -> (Value -> Bool) -> Spec
testObject gen fp p = do
  scm <- loadAndParse $ prependTestDir fp
  it ("Should validate valid objects" ++ ": " ++ fp) (yesProp gen p scm)
  it ("Should not validate invalid objects" ++ ": " ++ fp) (noProp gen (not . p) scm)

-- Tests for Object values that should always get invalidated.
testInvalidObject :: Gen Value -> FilePath -> Spec
testInvalidObject gen fp = do
  scm <- loadAndParse $ prependTestDir fp
  it ("Should not validate" ++ ": " ++ fp) (noProp gen (const True) scm)

-- "validation succeeded" property
yesProp :: Gen Value -> (Value -> Bool) -> Schema -> Property
yesProp gen p scm = property $ forAll gen prop
  where
    prop v = p v ==> isRight . runExcept . validate scm . encode $ v

-- "validation failed" property
noProp :: Gen Value -> (Value -> Bool) -> Schema -> Property
noProp gen p scm = property $ forAll gen prop
  where
    prop v = p v ==> isLeft . runExcept . validate scm . encode $ v

-- Returns true iff the value is an object with the given property and the
-- property-value satisfies the predicate.
hasProperty :: Text -> (Value -> Bool) -> Value -> Bool
hasProperty propName p (Object obj) = maybe False p $ lookup propName obj
hasProperty _ _ _ = False

-- Like hasProperty but is also true when the given property is absent.
hasOptionalProperty :: Text -> (Value -> Bool) -> Value -> Bool
hasOptionalProperty propName p (Object obj) = maybe True p $ lookup propName obj
hasOptionalProperty _ _ _ = False

loadAndParse :: FilePath -> SpecM () Schema
loadAndParse fp = do
  result <- runIO . runTestM . loadSchemaFromFile $ fp
  it ("Should parse: " ++ fp) (result `shouldNotSatisfy` isParseError)
  it ("Should build: " ++ fp) (result `shouldNotSatisfy` isSchemaError)
  case result of
    Left e -> error ("This should never happen: " ++ show e)
    Right scm -> pure scm

prependTestDir :: FilePath -> FilePath
prependTestDir = ("./conformance/validation/" ++)

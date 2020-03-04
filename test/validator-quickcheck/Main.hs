module Main where

import Control.Monad.Except (runExcept)
import Data.Aeson (Value, encode)
import Data.Aeson.Arbitrary
  ( RandomJSON (..),
    isArray,
    isBool,
    isNull,
    isNumber,
    isObject,
    isString,
  )
import Data.Either (isLeft, isRight)
import Data.Medea (Schema, loadSchemaFromFile, validate)
import Test.Hspec (Spec, describe, hspec, it, runIO, shouldNotSatisfy)
import Test.Hspec.Core.Spec (SpecM)
import Test.QuickCheck ((==>), arbitrary, forAll, property)
import TestM (isParseError, isSchemaError, runTestM)

main :: IO ()
main = hspec $ do
  describe "Any schema" . testAny $ "./conformance/validation/any.medea"
  describe "Null schema" . testSingular "./conformance/validation/null.medea" "null" $ isNull
  describe "Boolean schema" . testSingular "./conformance/validation/boolean.medea" "boolean" $ isBool
  describe "Number schema" . testSingular "./conformance/validation/number.medea" "number" $ isNumber
  describe "String schema" . testSingular "./conformance/validation/string.medea" "string" $ isString
  describe "Array schema" . testSingular "./conformance/validation/array.medea" "array" $ isArray
  describe "Object schema" . testSingular "./conformance/validation/object.medea" "object" $ isObject

-- Helpers

testAny :: FilePath -> Spec
testAny fp = do
  scm <- loadAndParse fp
  it ("Should validate anything: " ++ fp) (property . forAll arbitrary . go $ scm)
  where
    go scm (RandomJSON v) = isRight . runExcept . validate scm . encode $ v

testSingular :: FilePath -> String -> (Value -> Bool) -> Spec
testSingular fp name p = do
  scm <- loadAndParse fp
  it ("Should validate " ++ name ++ "s: " ++ fp) (property . forAll arbitrary . yesProp $ scm)
  it ("Should not validate non-" ++ name ++ "s: " ++ fp) (property . forAll arbitrary . noProp $ scm)
  where
    yesProp scm (RandomJSON v) = p v ==> isRight . runExcept . validate scm . encode $ v
    noProp scm (RandomJSON v) = (not . p $ v) ==> isLeft . runExcept . validate scm . encode $ v

loadAndParse :: FilePath -> SpecM () Schema
loadAndParse fp = do
  result <- runIO . runTestM . loadSchemaFromFile $ fp
  it ("Should parse: " ++ fp) (result `shouldNotSatisfy` isParseError)
  it ("Should build: " ++ fp) (result `shouldNotSatisfy` isSchemaError)
  case result of
    Left e -> error ("This should never happen: " ++ show e)
    Right scm -> pure scm

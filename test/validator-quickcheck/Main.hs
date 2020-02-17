module Main where

import Test.Hspec (Spec, hspec, describe, runIO, shouldNotSatisfy, it)
import Test.Hspec.Core.Spec (SpecM)
import Test.QuickCheck (property, forAll, arbitrary, (==>))
import Data.Either (isRight, isLeft)
import Data.Aeson (Value)

import Data.Medea.Loader (loadSchemaFromFile)
import Data.Medea.Validator (Schema, validateEither)
import Data.Aeson.Arbitrary (RandomJSON(..), 
                             isNull, isBool, isNumber, isString, isArray, isObject)
import TestM (runTestM, isParseError, isSchemaError)

main :: IO ()
main = hspec $ do
  describe "Null schema" . testSingular "./conformance/validation/null.medea" "null" $ isNull
  describe "Boolean schema" . testSingular "./conformance/validation/boolean.medea" "boolean" $ isBool
  describe "Number schema" . testSingular "./conformance/validation/number.medea" "number" $ isNumber
  describe "String schema" . testSingular  "./conformance/validation/string.medea" "string" $ isString
  describe "Array schema" . testSingular "./conformance/validation/array.medea" "array" $ isArray
  describe "Object schema" . testSingular "./conformance/validation/object.medea" "object" $ isObject

-- Helpers

testSingular :: FilePath -> String -> (Value -> Bool) -> Spec
testSingular fp name p = do
  scm <- loadAndParse fp
  it ("Should validate " ++ name ++ "s: " ++ fp) (property . forAll arbitrary . yesProp $ scm)
  it ("Should not validate non-" ++ name ++ "s: " ++ fp) (property . forAll arbitrary . noProp $ scm)
  where yesProp scm (RandomJSON v) = p v ==> isRight . validateEither scm $ v
        noProp scm (RandomJSON v) = (not . p $ v) ==> isLeft . validateEither scm $ v

loadAndParse :: FilePath -> SpecM () Schema
loadAndParse fp = do
  result <- runIO . runTestM . loadSchemaFromFile $ fp
  it ("Should parse: " ++ fp) (result `shouldNotSatisfy` isParseError)
  it ("Should build: " ++ fp) (result `shouldNotSatisfy` isSchemaError)
  case result of
    Left _ -> error "This should never happen"
    Right scm -> pure scm 

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
import Test.QuickCheck ((==>), arbitrary, forAll, property, quickCheck)
import qualified Test.QuickCheck.Gen as Gen
import TestM (isParseError, isSchemaError, runTestM)

import Debug.Trace (traceShow)

spy x = traceShow x x

main :: IO ()
main = hspec $ do
  -- describe "Any schema" . testAny $ "./conformance/validation/any.medea"
  -- describe "Null schema" . testSingular "./conformance/validation/null.medea" "null" $ isNull
  -- describe "Boolean schema" . testSingular "./conformance/validation/boolean.medea" "boolean" $ isBool
  -- describe "Number schema" . testSingular "./conformance/validation/number.medea" "number" $ isNumber
  -- describe "String schema" . testSingular "./conformance/validation/string.medea" "string" $ isString
  -- describe "Array schema" . testSingular "./conformance/validation/array.medea" "array" $ isArray
  -- describe "Object schema" . testSingular "./conformance/validation/object.medea" "object" $ isObject
  -- describe "Boolean/null schema" . testSingular "./conformance/validation/nullable-boolean.medea" "boolean/null" $ isNullOr isBool
  -- describe "Number/null schema" . testSingular "./conformance/validation/nullable-number.medea" "number/null" $ isNullOr isNumber
  -- describe "String/null schema" . testSingular "./conformance/validation/nullable-string.medea" "string/null" $ isNullOr isString
  -- describe "Array/null schema" . testSingular "./conformance/validation/nullable-array.medea" "array/null" $ isNullOr isArray
  -- describe "Object/null schema" . testSingular "./conformance/validation/nullable-object.medea" "object/null" $ isNullOr isObject
  describe "String with Values Schema" $ testStringVals "./conformance/validation/stringVals.medea" "string is one of [\"bar\", \"baz\"]" ["bar", "baz"] 

-- Helpers

isNullOr :: (Value -> Bool) -> (Value -> Bool)
isNullOr f = (||) <$> isNull <*> f

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

testStringVals :: FilePath -> String -> [ String ] -> Spec
testStringVals fp name validStrings = do
  scm <- loadAndParse fp
  pure ()
  it ("Should validate " ++ name ++ "s: " ++ fp) (property . forAll genString . validationIsCorrect $ scm)
  
  it ("Shouldn't validate " ++ name ++ "s: " ++ fp) (property . forAll genString . invalidationIsCorrect $ scm)
  where 
    validationIsCorrect scm s = s `elem` validStrings ==> isRight . runExcept . validate scm $ encode s 
    invalidationIsCorrect scm s  = not (s `elem` validStrings) ==> isLeft . runExcept . validate scm . encode $ s

    genString :: Gen.Gen String
    genString = Gen.oneof [ (Gen.elements validStrings), arbitrary]

loadAndParse :: FilePath -> SpecM () Schema
loadAndParse fp = do
  result <- runIO . runTestM . loadSchemaFromFile $ fp
  it ("Should parse: " ++ fp) (result `shouldNotSatisfy` isParseError)
  it ("Should build: " ++ fp) (result `shouldNotSatisfy` isSchemaError)
  case result of
    Left e -> error ("This should never happen: " ++ show e)
    Right scm -> pure scm

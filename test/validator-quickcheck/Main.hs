{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (Array, Object, ToJSON, Value, Value (..), encode)
import Data.Aeson.Arbitrary
  ( ObjGenOpts (..),
    arbitraryArray,
    arbitraryObj,
    arbitraryValue,
    isArray,
    isBool,
    isNull,
    isNumber,
    isObject,
    isString,
  )
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AKM
import Data.ByteString.Lazy (toStrict)
import Data.Either (isLeft, isRight)
import Data.Medea (Schema, loadSchemaFromFile, validate)
import Data.Text (Text)
import qualified Data.Vector as V
import Test.Hspec (Spec, describe, hspec, it, parallel, runIO, shouldNotSatisfy)
import Test.Hspec.Core.Spec (SpecM)
import Test.QuickCheck ((==>), Gen, Property, arbitrary, forAll, property)
import qualified Test.QuickCheck.Gen as Gen
import TestM (isParseError, isSchemaError)
import Prelude hiding (lookup)

main :: IO ()
main = hspec . parallel $ do
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
  describe "String with Values Schema" $ do
    testStringVals "stringVals.medea" ["bar", "baz"]
    testStringVals "stringVals2.medea" ["accountant", "barber", "bishop", "baker"]
  describe "Object schema with 1 property and no additional allowed" $ do
    testObject
      ObjTestParams
        { objTestOpts = ObjGenOpts ["foo"] [] 0 2,
          objTestPath = "1-property-no-additional-1.medea",
          objTestPred = hasProperty "foo" isBool,
          objAdditionalPred = const False
        }
    testObject
      ObjTestParams
        { objTestOpts = ObjGenOpts ["foo"] [] 0 2,
          objTestPath = "1-property-no-additional-2.medea",
          objTestPred = hasProperty "foo" isNull,
          objAdditionalPred = const False
        }
    testObject
      ObjTestParams
        { objTestOpts = ObjGenOpts ["foo"] [] 0 2,
          objTestPath = "1-property-no-additional-3.medea",
          objTestPred = hasProperty "foo" isArray,
          objAdditionalPred = const False
        }
  describe "Object schema with 1 property and additional allowed" $ do
    testObject
      ObjTestParams
        { objTestOpts = ObjGenOpts ["foo"] [] 0 3,
          objTestPath = "1-property-additional-1.medea",
          objTestPred = hasProperty "foo" isString,
          objAdditionalPred = const True
        }
    testObject
      ObjTestParams
        { objTestOpts = ObjGenOpts ["foo"] [] 0 3,
          objTestPath = "1-property-additional-2.medea",
          objTestPred = hasProperty "foo" isNumber,
          objAdditionalPred = const True
        }
    testObject
      ObjTestParams
        { objTestOpts = ObjGenOpts ["foo"] [] 0 3,
          objTestPath = "1-property-additional-3.medea",
          objTestPred = hasProperty "foo" isObject,
          objAdditionalPred = const True
        }
  describe "Object schema with 3 properties and no additional allowed" $ do
    testObject
      ObjTestParams
        { objTestOpts = ObjGenOpts ["foo", "bar", "bazz"] [] 0 1,
          objTestPath = "3-property-no-additional-1.medea",
          objTestPred =
            hasProperty "foo" (isNumber .|| isArray)
              .&& hasProperty "bazz" (isNull .|| isBool),
          objAdditionalPred = const False
        }
    testObject
      ObjTestParams
        { objTestOpts = ObjGenOpts ["bar", "bazz"] ["foo"] 0 1,
          objTestPath = "3-property-no-additional-2.medea",
          objTestPred =
            hasOptionalProperty "foo" (isNumber .|| isArray)
              .&& hasProperty "bazz" (isNull .|| isBool),
          objAdditionalPred = const False
        }
  describe "Object schema with 3 properties and additional allowed" $ do
    testObject
      ObjTestParams
        { objTestOpts = ObjGenOpts ["foo", "bar", "bazz"] [] 0 3,
          objTestPath = "3-property-additional-allowed-1.medea",
          objTestPred = hasProperty "foo" isBool .&& hasProperty "bazz" isString,
          objAdditionalPred = const True
        }
    testObject
      ObjTestParams
        { objTestOpts = ObjGenOpts ["bar", "bazz"] ["foo"] 0 3,
          objTestPath = "3-property-additional-allowed-2.medea",
          objTestPred = hasOptionalProperty "foo" isNumber .&& hasProperty "bazz" isNull,
          objAdditionalPred = const True
        }
  describe "Object schema with additional property schema" $ do
    testObject
      ObjTestParams
        { objTestOpts = ObjGenOpts [] [] 0 3,
          objTestPath = "map-number-bool.medea",
          objTestPred = const True,
          objAdditionalPred = isNumber .|| isBool
        }
    testObject
      ObjTestParams
        { objTestOpts = ObjGenOpts ["foo"] [] 0 3,
          objTestPath = "map-with-1-specified.medea",
          objTestPred = hasProperty "foo" (isArray .|| isObject),
          objAdditionalPred = isNumber .|| isBool
        }
    testObject
      ObjTestParams
        { objTestOpts = ObjGenOpts ["foo"] ["bazz"] 0 3,
          objTestPath = "map-with-2-specified.medea",
          objTestPred = hasProperty "foo" (isArray .|| isObject),
          objAdditionalPred = isNumber .|| isBool
        }
  describe "Array schema with element_type only" $ do
    testList
      ListTestParams
        { listTestOpts = (0, 3),
          listTestPath = "list-1.medea",
          elementPred = isNumber .|| isBool .|| isObject,
          lenPred = const True
        }
    testList
      ListTestParams
        { listTestOpts = (1, 3),
          listTestPath = "list-2.medea",
          elementPred = isNumber .|| isBool .|| isObject,
          lenPred = const True
        }
  describe "Array schema with length spec only" $ do
    testList
      ListTestParams
        { listTestOpts = (1, 6),
          listTestPath = "list-3.medea",
          elementPred = const True,
          lenPred = arrayLenGE 2
        }
    testList
      ListTestParams
        { listTestOpts = (1, 6),
          listTestPath = "list-4.medea",
          elementPred = const True,
          lenPred = arrayLenLE 5
        }
    testList
      ListTestParams
        { listTestOpts = (1, 6),
          listTestPath = "list-5.medea",
          elementPred = const True,
          lenPred = arrayLenLE 5 .&& arrayLenGE 3
        }
  describe "Array schema with length and element type" $ do
    testList
      ListTestParams
        { listTestOpts = (1, 4),
          listTestPath = "list-6.medea",
          elementPred = isNull .|| isBool .|| isNumber,
          lenPred = arrayLenGE 2 .&& arrayLenLE 3
        }
    testList
      ListTestParams
        { listTestOpts = (1, 4),
          listTestPath = "list-7.medea",
          elementPred = isNull .|| isBool .|| isNumber,
          lenPred = arrayLenGE 2 .&& arrayLenLE 3
        }
  describe "Array schema with tuple spec" $ do
    testTuple
      TupleTestParams
        { tupleTestOpts = (3, 4),
          tupleTestPath = "3-tuple.medea",
          tuplePreds = [isNumber .|| isArray, isBool, const True]
        }
    testTuple
      TupleTestParams
        { tupleTestOpts = (1, 3),
          tupleTestPath = "2-tuple.medea",
          tuplePreds = [isObject .|| isNull, isString .|| isNumber]
        }

data ObjTestParams = ObjTestParams
  { objTestOpts :: ObjGenOpts,
    objTestPath :: FilePath,
    objTestPred :: Object -> Bool,
    -- | The predice to be used on additional properties
    objAdditionalPred :: Value -> Bool
  }

data ListTestParams = ListTestParams
  { listTestOpts :: (Int, Int),
    listTestPath :: FilePath,
    elementPred :: Value -> Bool,
    lenPred :: Array -> Bool
  }

data TupleTestParams = TupleTestParams
  { tupleTestOpts :: (Int, Int),
    tupleTestPath :: FilePath,
    tuplePreds :: [Value -> Bool]
  }

-- Helpers

(.||) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
f .|| g = (||) <$> f <*> g

(.&&) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
f .&& g = (&&) <$> f <*> g

testAny :: FilePath -> Spec
testAny fp = do
  scm <- loadAndParse $ prependTestDir fp
  it ("Should validate anything: " ++ fp) (validationSuccess arbitraryValue (const True) scm)

testSingular :: FilePath -> String -> (Value -> Bool) -> Spec
testSingular fp name p = do
  scm <- loadAndParse $ prependTestDir fp
  it ("Should validate " ++ name ++ "s: " ++ fp) (validationSuccess arbitraryValue p scm)
  it ("Should not validate non-" ++ name ++ "s: " ++ fp) (validationFail arbitraryValue (not . p) scm)

testObject :: ObjTestParams -> Spec
testObject (ObjTestParams opts fp p extraPred) = do
  scm <- loadAndParse $ prependTestDir fp
  it ("Should validate valid objects" ++ ": " ++ fp) (validationSuccess gen p' scm)
  it ("Should not validate invalid objects" ++ ": " ++ fp) (validationFail gen (not . p') scm)
  where
    gen = arbitraryObj opts
    p' = p .&& makeMapPred opts extraPred

testList :: ListTestParams -> Spec
testList (ListTestParams opts fp pTypes pLen) = do
  scm <- loadAndParse $ prependTestDir fp
  it ("Should validate valid lists" ++ ": " ++ fp) (validationSuccess gen p scm)
  it ("Should not validate invalid lists" ++ ": " ++ fp) (validationFail gen (not . p) scm)
  where
    gen = arbitraryArray opts
    p = all pTypes .&& pLen

testTuple :: TupleTestParams -> Spec
testTuple (TupleTestParams opts fp preds) = do
  scm <- loadAndParse $ prependTestDir fp
  it ("Should validate valid tuples" ++ ": " ++ fp) (validationSuccess gen p scm)
  it ("Should not validate invalid tuples" ++ ": " ++ fp) (validationFail gen (not . p) scm)
  where
    gen = arbitraryArray opts
    p arr = (and . zipWith ($) preds . V.toList $ arr) && (V.length arr == length preds)

-- "validation succeeded" property
validationSuccess :: (ToJSON a, Show a) => Gen a -> (a -> Bool) -> Schema -> Property
validationSuccess gen p scm = property $ forAll gen prop
  where
    prop v = p v ==> isRight . validate scm . toStrict . encode $ v

-- "validation failed" property
validationFail :: (ToJSON a, Show a) => Gen a -> (a -> Bool) -> Schema -> Property
validationFail gen p scm = property $ forAll gen prop
  where
    prop v = p v ==> isLeft . validate scm . toStrict . encode $ v

-- Returns true iff the value is an object with the given property and the
-- property-value satisfies the predicate.
hasProperty :: Text -> (Value -> Bool) -> Object -> Bool
hasProperty propName p obj = maybe False p $ AKM.lookup (AesonKey.fromText propName) obj

-- Like hasProperty but is also true when the given property is absent.
hasOptionalProperty :: Text -> (Value -> Bool) -> Object -> Bool
hasOptionalProperty propName p obj = maybe True p $ AKM.lookup (AesonKey.fromText propName) obj

makeMapPred :: ObjGenOpts -> (Value -> Bool) -> Object -> Bool
makeMapPred (ObjGenOpts props optProps _ _) p = all p . AKM.filterWithKey (\k _ -> k `notElem` specifiedProps)
  where
    specifiedProps = fmap (AesonKey.fromText) $ props ++ optProps

testStringVals :: FilePath -> [String] -> Spec
testStringVals fp validStrings = do
  scm <- loadAndParse $ prependTestDir fp
  it ("Should validate only strings in " ++ show validStrings ++ ": " ++ fp) (validationSuccess genString p scm)
  it ("Shouldn't validate strings not in " ++ show validStrings ++ "s: " ++ fp) (validationFail genString (not . p) scm)
  where
    genString :: Gen.Gen String
    genString = Gen.oneof [Gen.elements validStrings, arbitrary]
    p = (`elem` validStrings)

loadAndParse :: FilePath -> SpecM () Schema
loadAndParse fp = do
  result <- runIO . loadSchemaFromFile $ fp
  it ("Should parse: " ++ fp) (result `shouldNotSatisfy` isParseError)
  it ("Should build: " ++ fp) (result `shouldNotSatisfy` isSchemaError)
  case result of
    Left e -> error ("This should never happen: " <> show e)
    Right scm -> pure scm

prependTestDir :: FilePath -> FilePath
prependTestDir = ("./conformance/validation/" ++)

arrayLenGE :: Int -> Array -> Bool
arrayLenGE len arr = V.length arr >= len

arrayLenLE :: Int -> Array -> Bool
arrayLenLE len arr = V.length arr <= len

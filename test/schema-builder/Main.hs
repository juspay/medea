module Main where

import Data.Either (isRight)
import Data.Foldable (traverse_)
import Data.Medea (loadSchemaFromFile)
import Test.Hspec
  ( Spec,
    describe,
    hspec,
    it,
    runIO,
    shouldSatisfy,
  )
import TestM (isSchemaError, runTestM)
import TestM.Util (listMedeaFiles)

main :: IO ()
main = do
  let failDir = "./conformance/schema-builder/fail"
  let passDir = "./conformance/schema-builder/pass"
  failFiles <- listMedeaFiles failDir
  passFiles <- listMedeaFiles passDir
  hspec $ do
    describe "Invalid schemata cases" . traverse_ makeFailTest $ failFiles
    describe "Valid schemata cases" . traverse_ makePassTest $ passFiles

-- Helpers

makeFailTest :: FilePath -> Spec
makeFailTest fp = do
  result <- runIO . runTestM . loadSchemaFromFile $ fp
  it ("Shouldn't build: " ++ fp) (result `shouldSatisfy` isSchemaError)

makePassTest :: FilePath -> Spec
makePassTest fp = do
  result <- runIO . runTestM . loadSchemaFromFile $ fp
  it ("Should build: " ++ fp) (result `shouldSatisfy` isRight)

module Main where

import Data.Foldable (traverse_)
import Data.Medea (loadSchemaFromFile)
import Test.Hspec
  ( Spec,
    describe,
    hspec,
    it,
    runIO,
    shouldNotSatisfy,
    shouldSatisfy,
  )
import TestM.Util (listMedeaFiles)
import TestM (isSchemaError, runTestM)

main :: IO ()
main = do
  let prefix = "./conformance/schema-builder"
  testFiles <- listMedeaFiles prefix
  hspec . describe "Invalid schemata cases" . traverse_ makeSchemaTest $ testFiles

-- Helpers

makeSchemaTest :: FilePath -> Spec
makeSchemaTest fp = do
  result <- runIO . runTestM . loadSchemaFromFile $ fp
  it ("Shouldn't build: " ++ fp) (result `shouldSatisfy` isSchemaError)

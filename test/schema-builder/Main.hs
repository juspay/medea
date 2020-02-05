module Main where

import Data.List (sort)
import Test.Hspec (Spec, 
                   hspec, describe, runIO, it, shouldSatisfy, shouldNotSatisfy)
import System.FilePath (isExtensionOf, (</>))
import System.Directory (listDirectory)
import Data.Foldable (traverse_)

import TestM (runTestM, isParseError, isSchemaError)
import Data.Medea.Loader (loadSchemaFromFile)

main :: IO ()
main = do
  let prefix = "./conformance/schema-builder"
  testFiles <- fmap (prefix </>) . sort . filter (isExtensionOf ".medea") <$> listDirectory prefix
  hspec . describe "Invalid schemata cases" . traverse_ makeSchemaTest $ testFiles

-- Helpers

makeSchemaTest :: FilePath -> Spec
makeSchemaTest fp = do
  result <- runIO . runTestM . loadSchemaFromFile $ fp
  it ("Should parse: " ++ fp) (result `shouldNotSatisfy` isParseError)
  it ("Shouldn't build: " ++ fp) (result `shouldSatisfy` isSchemaError)

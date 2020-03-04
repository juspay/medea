module Main where

import Data.Foldable (traverse_)
import Data.List (sort)
import Data.Medea (loadSchemaFromFile)
import System.Directory (listDirectory)
import System.FilePath ((</>), isExtensionOf)
import Test.Hspec
  ( Spec,
    describe,
    hspec,
    it,
    runIO,
    shouldNotSatisfy,
    shouldSatisfy,
  )
import TestM (isParseError, isSchemaError, runTestM)

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

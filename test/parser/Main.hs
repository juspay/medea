module Main where

import Data.Foldable (traverse_)
import Data.List (sort)
import Data.Medea (loadSchemaFromFile)
import System.Directory (listDirectory)
import System.FilePath ((</>), isExtensionOf)
import Test.Hspec (Spec, describe, hspec, it, runIO, shouldSatisfy)
import TestM (isParseError, runTestM)

main :: IO ()
main = do
  let prefix = "./conformance/parser"
  testFiles <- fmap (prefix </>) . sort . filter (isExtensionOf ".medea") <$> listDirectory prefix
  hspec . describe "Invalid parse cases" . traverse_ makeParseTest $ testFiles

-- Helpers

makeParseTest :: FilePath -> Spec
makeParseTest fp = do
  result <- runIO . runTestM . loadSchemaFromFile $ fp
  it ("Shouldn't parse: " ++ fp) (result `shouldSatisfy` isParseError)

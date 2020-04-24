module Main where

import Data.Foldable (traverse_)
import Data.Medea (loadSchemaFromFile)
import Test.Hspec (Spec, describe, hspec, it, runIO, shouldNotSatisfy, shouldSatisfy)
import TestM (isParseError, runTestM)
import TestM.Util (listMedeaFiles)

main :: IO ()
main = do
  let failDir = "./conformance/parser/fail"
      passDir = "./conformance/parser/pass"
  failTestFiles <- listMedeaFiles failDir
  passTestFiles <- listMedeaFiles passDir
  hspec . describe "Invalid parse cases" . traverse_ makeParseTestFail $ failTestFiles
  hspec . describe "Valid parse cases" . traverse_ makeParseTestPass $ passTestFiles

-- Helpers

makeParseTestFail :: FilePath -> Spec
makeParseTestFail fp = do
  result <- runIO . runTestM . loadSchemaFromFile $ fp
  it ("Shouldn't parse: " ++ fp) (result `shouldSatisfy` isParseError)

makeParseTestPass :: FilePath -> Spec
makeParseTestPass fp = do
  result <- runIO . runTestM . loadSchemaFromFile $ fp
  it ("Should parse: " ++ fp) (result `shouldNotSatisfy` isParseError)

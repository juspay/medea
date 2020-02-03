module Main where

import Data.Either (isLeft, isRight)
import Test.Hspec (Spec, hspec, runIO, describe, it, shouldSatisfy)
import Data.Foldable (traverse_)
import System.Directory (listDirectory)
import System.FilePath (isExtensionOf, (</>))
import Data.List (sort)

import Data.Medea (loadSchema)

main :: IO ()
main = do
  let passPrefix = "./conformance/loader/valid"
  let failPrefix = "./conformance/loader/invalid"
  passFiles <- fmap (passPrefix </>) . sort . filter (isExtensionOf ".medea") <$> listDirectory passPrefix
  failFiles <- fmap (failPrefix </>) . sort . filter (isExtensionOf ".medea") <$> listDirectory failPrefix
  hspec $ do
    describe "Valid cases for loading" (traverse_ describePassing passFiles)
    describe "Invalid cases for loading" (traverse_ describeFailing failFiles)

-- Helpers

describePassing :: FilePath -> Spec
describePassing fp = do
  result <- runIO . loadSchema $ fp
  it ("Should load: " ++ fp) (result `shouldSatisfy` isRight)

describeFailing :: FilePath -> Spec
describeFailing fp = do
  result <- runIO . loadSchema $ fp
  it ("Shouldn't load: " ++ fp) (result `shouldSatisfy` isLeft)

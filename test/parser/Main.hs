{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Except (MonadError, ExceptT, runExceptT)
import Data.Either (isLeft)
import Data.List (sort)
import Test.Hspec (Spec, hspec, describe, runIO, it, shouldSatisfy)
import System.FilePath (isExtensionOf, (</>))
import Data.Foldable (traverse_)
import System.Directory (listDirectory)

import Data.Medea.Loader (LoaderError, loadSchemaFromFile)

main :: IO ()
main = do
  let prefix = "./conformance/parser"
  testFiles <- fmap (prefix </>) . sort . filter (isExtensionOf ".medea") <$> listDirectory prefix
  hspec . describe "Invalid parse cases" . traverse_ makeParseTest $ testFiles

-- Helpers

newtype TestM a = TestM (ExceptT LoaderError IO a)
  deriving (Functor, Applicative, Monad, MonadError LoaderError, MonadIO)

runTestM :: TestM a -> IO (Either LoaderError a)
runTestM (TestM comp) = runExceptT comp

makeParseTest :: FilePath -> Spec
makeParseTest fp = do
  result <- runIO . runTestM . loadSchemaFromFile $ fp
  it ("Shouldn't parse: " ++ fp) (result `shouldSatisfy` isLeft)

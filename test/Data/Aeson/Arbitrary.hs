{-# LANGUAGE TypeApplications #-}

module Data.Aeson.Arbitrary where

import Control.Monad (replicateM)
import Test.QuickCheck (Arbitrary(..), Gen)
import Test.QuickCheck.Gen (choose, chooseAny)
import Data.Aeson (Value(..))
import Control.Monad.Trans (lift)
import Control.Monad.Reader (ReaderT, runReaderT, asks, local)
import Test.QuickCheck.Instances.Text ()
import Test.QuickCheck.Instances.Scientific ()

import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM

newtype RandomJSON = RandomJSON { toValue :: Value }
  deriving (Show)

instance Arbitrary RandomJSON where
  arbitrary = RandomJSON <$> runReaderT makeRandomValue 5 -- recursion depth

isNull :: Value -> Bool
isNull Null = True
isNull _ = False

isBool :: Value -> Bool
isBool (Bool _) = True
isBool _ = False

isNumber :: Value -> Bool
isNumber (Number _) = True
isNumber _ = False

isString :: Value -> Bool
isString (String _) = True
isString _ = False

isArray :: Value -> Bool
isArray (Array _) = True
isArray _ = False

isObject :: Value -> Bool
isObject (Object _) = True
isObject _ = False

-- Helpers

makeRandomValue :: ReaderT Word Gen Value
makeRandomValue = do
  reachedMaxDepth <- asks (== 0)
  choice <- lift . choose @Word $ (0, if reachedMaxDepth then 3 else 5)
  case choice of
    0 -> pure Null
    1 -> Bool <$> lift chooseAny
    2 -> Number <$> lift arbitrary
    3 -> String <$> lift arbitrary
    4 -> do
      len <- lift . choose $ (0, 10)
      Array <$> V.replicateM len (local dec makeRandomValue)
    _ -> do
      entryCount <- lift . choose $ (0, 10)
      keyVals <- replicateM entryCount ((,) <$> lift arbitrary <*> local dec makeRandomValue)
      pure . Object . HM.fromList $ keyVals
  where dec = subtract 1

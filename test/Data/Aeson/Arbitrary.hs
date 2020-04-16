{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aeson.Arbitrary where

import Control.Monad (replicateM, filterM)
import Test.QuickCheck (Arbitrary(..), Gen)
import Test.QuickCheck.Gen (choose, chooseAny)
import Data.Aeson (Value(..))
import Control.Monad.Trans (lift)
import Control.Monad.Reader (ReaderT, runReaderT, asks, local)
import Test.QuickCheck.Instances.Text ()
import Test.QuickCheck.Instances.Scientific ()

import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)

-- Takes 4 fields:
-- required properties,
-- optional properties,
-- minimum additional properties &
-- maximum additional properties.
data ObjGenOpts = ObjGenOpts [Text] [Text] Int Int

arbitraryValue :: Gen Value
arbitraryValue = runReaderT makeRandomValue 5 -- recursion depth

arbitraryObj :: ObjGenOpts -> Gen Value
arbitraryObj opts = runReaderT (makeRandomObject opts) 2

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
    _ -> makeRandomObject (ObjGenOpts [] [] 0 10)

makeRandomObject :: ObjGenOpts -> ReaderT Word Gen Value
makeRandomObject (ObjGenOpts props optionalProps minAdditional maxAdditional) = do
  entryCount <- lift $ choose (minAdditional, maxAdditional)
  genKeys <- replicateM entryCount $ lift arbitrary
  someOptionalProps <- filterM (\_ -> lift arbitrary) optionalProps
  let keys = genKeys ++ props ++ someOptionalProps
  keyVals <- mapM (\x -> (x,) <$> local dec makeRandomValue) keys
  pure . Object . HM.fromList $ keyVals

dec :: Word -> Word
dec = subtract 1

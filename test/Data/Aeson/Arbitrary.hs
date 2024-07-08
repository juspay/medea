{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Data.Aeson.Arbitrary
  ( ObjGenOpts(..),
    arbitraryArray,
    arbitraryObj,
    arbitraryValue,
    isArray,
    isBool,
    isNull,
    isNumber,
    isObject,
    isString,
  )
where

import Control.Monad (filterM, replicateM)
import Control.Monad.Reader (ReaderT, asks, local, runReaderT)
import Control.Monad.Trans (lift)
import Data.Aeson (Array, Object, Value (..))
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Vector as V
import Test.QuickCheck (Arbitrary (..), Gen)
import Test.QuickCheck.Gen (choose)
import Test.QuickCheck.Instances.Scientific ()
import Test.QuickCheck.Instances.Text ()

-- Takes 4 fields:
-- required properties,
-- optional properties,
-- minimum additional properties &
-- maximum additional properties.
data ObjGenOpts = ObjGenOpts [Text] [Text] Int Int

arbitraryValue :: Gen Value
arbitraryValue = runReaderT makeRandomValue 5 -- recursion depth

arbitraryObj :: ObjGenOpts -> Gen Object
arbitraryObj opts = runReaderT (makeRandomObject opts) 2

arbitraryArray :: (Int, Int) -> Gen Array
arbitraryArray range = runReaderT (makeRandomArray range) 2

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
    1 -> Bool <$> lift arbitrary
    2 -> Number <$> lift arbitrary
    3 -> String <$> lift arbitrary
    4 -> Array <$> makeRandomArray (0, 10)
    _ -> Object <$> makeRandomObject (ObjGenOpts [] [] 0 10)

makeRandomArray :: (Int, Int) -> ReaderT Word Gen Array
makeRandomArray range = do
  len <- lift . choose $ range
  V.replicateM len (local dec makeRandomValue)

makeRandomObject :: ObjGenOpts -> ReaderT Word Gen Object
makeRandomObject (ObjGenOpts props optionalProps minAdditional maxAdditional) = do
  entryCount <- lift $ choose (minAdditional, maxAdditional)
  genKeys <- replicateM entryCount $ lift arbitrary
  someOptionalProps <- filterM (\_ -> lift arbitrary) optionalProps
  let keys = genKeys ++ props ++ someOptionalProps
  keyVals <- mapM (\x -> (x,) <$> local dec makeRandomValue) keys
  pure . AKM.fromHashMapText . HM.fromList $ keyVals

dec :: Word -> Word
dec = subtract 1

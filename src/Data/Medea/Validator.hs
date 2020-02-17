{-# LANGUAGE FlexibleContexts #-}

module Data.Medea.Validator 
(
  ValidatorError(..),
  Schema,
  validate, validateEither
) where

import Data.Functor (($>))
import Control.Monad (unless, foldM)
import Algebra.Graph.Acyclic.AdjacencyMap (AdjacencyMap, postSet)
import Control.Monad.Reader (MonadReader, runReaderT, asks)
import Data.Aeson (Value(..))
import Control.Monad.Except (MonadError(..), runExcept)

import Data.Medea.JSONType (JSONType, typeOf)
import Data.Medea.Parser.Identifier (startIdentifier)
import Data.Medea.Analysis (TypeNode(..))
import Data.Medea.Schema (Schema(..))

data ValidatorError =
  -- | We got a type different to what we were expecting.
  WrongType Value -- ^ context 
            JSONType | -- ^ expected type
  -- | Doesn't type according to a sum specification
  NotSumTypeMember Value -- ^ context
  deriving (Eq)

validate :: (MonadError ValidatorError m) => 
  Schema -> Value -> m ()
validate scm v = runReaderT (checkTypes v startingNode) (typeGraph scm)

validateEither :: Schema -> Value -> Either ValidatorError ()
validateEither scm = runExcept . validate scm

-- Helpers

startingNode :: TypeNode
startingNode = CustomNode startIdentifier 

{-# INLINE checkTypes #-}
checkTypes :: (MonadReader (AdjacencyMap TypeNode) m, MonadError ValidatorError m) => 
  Value -> TypeNode -> m ()
checkTypes v tn = case tn of
  AnyNode -> pure ()
  PrimitiveNode t -> unless (typeOf v == t) (throwError . WrongType v $ t)
  CustomNode _ -> do
    neighbourhood <- asks (postSet tn)
    -- This will fail on an empty neighbourhood.
    -- However, this is only possible for AnyNode or PrimitiveNode.
    -- Thus, should not happen.
    someSucceeded <- foldM go False neighbourhood
    unless someSucceeded (throwError . NotSumTypeMember $ v)
  where go True _ = pure True
        go False tn' = catchError (checkTypes v tn' $> True) (\_ -> pure False)

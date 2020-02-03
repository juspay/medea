{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Medea.Identifier where

import Prelude hiding (length)
import Control.Monad.Except (MonadError(..))
import Data.Hashable (Hashable)
import Data.ByteString.Short (length)
import Data.Text.Short (ShortText, toShortByteString, isPrefixOf)

import qualified Data.Vector as V

import Data.Medea.Primitive (Primitive(..))
import Data.Medea.Types.Lines (LineNumber)
import Data.Medea.Error (LoaderError, SemanticError(..), throwSemanticError)

newtype Identifier = Identifier ShortText
  deriving (Eq, Hashable)

makeIdentifier :: (MonadError LoaderError m) => 
  LineNumber -> ShortText -> m Identifier
makeIdentifier lineNum st = do
  let bytely = toShortByteString st
  if length bytely > 32
  then throwSemanticError . IdentifierTooLong $ lineNum
  else pure . Identifier $ st

isReserved :: Identifier -> Bool
isReserved (Identifier st) = "$" `isPrefixOf` st

isStarting :: Identifier -> Bool
isStarting (Identifier st) = "$start" == st

isPrimitive :: Identifier -> Bool
isPrimitive (Identifier st) = V.elem st primitives
  where primitives = V.fromList ["$null",
                                 "$number",
                                 "$boolean",
                                 "$string",
                                 "$object",
                                 "$array"]

start :: Identifier
start = Identifier "$start"

-- checked _externally_!
primFromIdentifier :: Identifier -> Primitive
primFromIdentifier (Identifier st) = case st of
  "$null" -> NullT
  "$number" -> NumberT
  "$boolean" -> BooleanT
  "$string" -> StringT
  "$object" -> ObjectT
  _ -> ArrayT

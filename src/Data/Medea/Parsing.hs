{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Medea.Parsing where

import Data.Char (GeneralCategory(..), generalCategory)
import Control.Monad.State.Strict (MonadState(..), modify)
import Control.Monad.Except (MonadError(..))
import Data.Word (Word8)
import Streamly.Data.Unicode.Stream (decodeUtf8)
import Streamly (SerialT, MonadAsync)
import Data.Text.Short (ShortText, pack, isPrefixOf, toByteString)

import qualified Data.ByteString as BS
import qualified Streamly.Prelude as SP
import qualified Streamly.Data.Fold as SDF

import Data.Medea.Identifier (Identifier(..))
import Data.Medea.Primitive (Primitive(..))

newtype ParserState = ParserState {
  lineNumber :: Word
}

data ParserError = ParserError {
  context :: Word,
  err :: ParserErrorType
}

data ParserErrorType = 
  -- got some non-UTF8 encoding
  NotUTF8 |
  -- someone fed us an identifier of excessive length
  IdentifierTooLong Int | -- length we encountered
  -- attempt to define a reserved identifier
  ReservedName ShortText -- the string they tried to use

-- Parser innards

data Token = 
  SchemataMarker |
  SchemaMarker |
  TypeMarker |
  PrimitiveTypeMarker Primitive |
  InitialSchemaMarker |
  SchemaName Identifier
  deriving (Eq)

makeToken :: (MonadError ParserError m) => 
  (Word, ShortText) -> m (Word, Token)
makeToken (line, st)
  | st == "$schemata" = pure (line, SchemataMarker)
  | st == "$schema" = pure (line, SchemaMarker)
  | st == "$type" = pure (line, TypeMarker)
  | st == "$null" = pure (line, PrimitiveTypeMarker NullT)
  | st == "$boolean" = pure (line, PrimitiveTypeMarker BooleanT)
  | st == "$object" = pure (line, PrimitiveTypeMarker ObjectT)
  | st == "$number" = pure (line, PrimitiveTypeMarker NumberT)
  | st == "$string" = pure (line, PrimitiveTypeMarker StringT)
  | st == "$array" = pure (line, PrimitiveTypeMarker ArrayT)
  | st == "$start" = pure (line, InitialSchemaMarker)
  | "$" `isPrefixOf` st = throwError . ParserError line . ReservedName $ st
  | otherwise = do
      let len = BS.length . toByteString $ st
      if len <= 32
      then pure (line, SchemaName . Identifier $ st)
      else throwError . ParserError line . IdentifierTooLong $ len

tokenize :: (MonadAsync m, MonadError ParserError m, MonadState Word m) => 
  SerialT m Word8 -> SerialT m (Word, Token)
tokenize = SP.mapM makeToken . SP.wordsBy isNewline gatherString . decodeUtf8
  where isNewline = (LineSeparator ==) . generalCategory
        gatherString = SDF.mapM go . fmap pack $ SDF.toList
        go st = do
          line <- get
          modify (+ 1)
          pure (line, st)

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Medea.Parser.Primitive where

import Prelude hiding (head)
import Control.Monad (when, replicateM_)
import Data.Char (isDigit, isSeparator, isControl)
import Data.Maybe (isJust)
import Data.Text (Text, cons, head, unpack, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Functor (($>))
import Text.Megaparsec (chunk, customFailure, single, some, manyTill,
                        takeWhile1P, (<|>))
import Text.Megaparsec.Char (char, eol)
import Text.Megaparsec.Char.Lexer (charLiteral)

import qualified Data.ByteString as BS

import Data.Medea.JSONType (JSONType(..))
import Data.Medea.Parser.Types (MedeaParser, ParseError(..))

-- Identifier
newtype Identifier = Identifier { toText :: Text }
  deriving (Eq, Ord, Show)

parseIdentifier :: MedeaParser Identifier
parseIdentifier = do
  ident <- takeWhile1P (Just "Non-separator") (not . isSeparatorOrControl)
  checkedConstruct Identifier ident

startIdentifier :: Identifier
startIdentifier = Identifier "$start"

newtype ReservedIdentifier = ReservedIdentifier Text
  deriving (Eq)

parseReserved :: MedeaParser ReservedIdentifier
parseReserved = do
  lead <- single '$'
  rest <- takeWhile1P Nothing (not . isSeparatorOrControl)
  let ident = cons lead rest
  checkedConstruct ReservedIdentifier ident

parseSchemaHeader :: MedeaParser ReservedIdentifier
parseSchemaHeader = parseReservedChunk "schema"

parseTypeHeader :: MedeaParser ReservedIdentifier
parseTypeHeader = parseReservedChunk "type"

parseLengthHeader :: MedeaParser ReservedIdentifier
parseLengthHeader = parseReservedChunk "length"

parseMinimumHeader :: MedeaParser ReservedIdentifier
parseMinimumHeader = parseReservedChunk "minimum"

parseMaximumHeader :: MedeaParser ReservedIdentifier
parseMaximumHeader = parseReservedChunk "maximum"

tryReserved :: Identifier -> Maybe ReservedIdentifier
tryReserved (Identifier ident) =
  if head ident == '$'
  then Just (ReservedIdentifier ident)
  else Nothing

forgetReserved :: ReservedIdentifier -> Identifier
forgetReserved (ReservedIdentifier ident) = Identifier ident

newtype PrimTypeIdentifier = PrimTypeIdentifier { typeOf :: JSONType }
  deriving (Eq)

parsePrimType :: MedeaParser PrimTypeIdentifier
parsePrimType = PrimTypeIdentifier <$>
                (chunk "$null" $> JSONNull <|>
                 chunk "$boolean" $> JSONBoolean <|>
                 chunk "$object" $> JSONObject <|>
                 chunk "$array" $> JSONArray <|>
                 chunk "$number" $> JSONNumber <|>
                 chunk "$string" $> JSONString)

tryPrimType :: Identifier -> Maybe PrimTypeIdentifier
tryPrimType (Identifier ident) = PrimTypeIdentifier <$>
  (case ident of
    "$null" -> Just JSONNull
    "$boolean" -> Just JSONBoolean
    "$object" -> Just JSONObject
    "$array" -> Just JSONArray
    "$number" -> Just JSONNumber
    "$string" -> Just JSONString
    _ -> Nothing)

forgetPrimType :: PrimTypeIdentifier -> Identifier
forgetPrimType ident = case typeOf ident of
  JSONNull -> Identifier "$null"
  JSONBoolean -> Identifier "$boolean"
  JSONObject -> Identifier "$object"
  JSONArray -> Identifier "$array"
  JSONNumber -> Identifier "$number"
  JSONString -> Identifier "$string"

isReserved :: Identifier -> Bool
isReserved = isJust . tryReserved

isStartIdent :: Identifier -> Bool
isStartIdent = (== Identifier "$start")

-- Natural Number
newtype Natural = Natural { toInteger :: Integer }
  deriving (Eq, Ord, Show)

parseNatural :: MedeaParser Natural
parseNatural = do
  digits <- takeWhile1P (Just "digits") isDigit
  when (head digits == '0') $
    customFailure . LeadingZero $ digits
  pure . Natural . read . unpack $ digits

-- String
newtype MedeaString = MedeaString { unwrap :: Text }
parseString :: MedeaParser MedeaString
parseString = do
  string <- char '"' *> manyTill charLiteral (char '"')
  pure . MedeaString . pack $ string

-- Helpers
checkedConstruct :: 
  (Text -> a) -> Text -> MedeaParser a
checkedConstruct f t =
  if (> 32) . BS.length . encodeUtf8 $ t
  then customFailure . IdentifierTooLong $ t
  else pure . f $ t

{-# INLINE parseReservedChunk #-}
parseReservedChunk :: Text -> MedeaParser ReservedIdentifier
parseReservedChunk identName = do
  ident <- chunk $ "$" <> identName
  pure . ReservedIdentifier $ ident

{-# INLINE parseLine #-}
parseLine :: Int -> MedeaParser a -> MedeaParser a
parseLine spaces p = replicateM_ spaces (char ' ') *> p <* eol

isSeparatorOrControl :: Char -> Bool
isSeparatorOrControl c = isSeparator c || isControl c

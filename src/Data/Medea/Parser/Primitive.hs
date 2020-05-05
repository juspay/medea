{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Medea.Parser.Primitive
  ( Identifier (..),
    MedeaString (..),
    Natural,
    PrimTypeIdentifier (..),
    ReservedIdentifier (..),
    identFromReserved,
    isReserved,
    isStartIdent,
    parseIdentifier,
    parseKeyVal,
    parseLine,
    parseNatural,
    parseReserved,
    parseString,
    tryPrimType,
  )
where

import Control.Monad (replicateM_, when)
import qualified Data.ByteString as BS
import Data.Char (isControl, isDigit, isSeparator)
import Data.Hashable (Hashable (..))
import Data.Maybe (isJust)
import Data.Medea.JSONType (JSONType (..))
import Data.Medea.Parser.Types (MedeaParser, ParseError (..))
import Data.Text (Text, head, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Text.Megaparsec
  ( customFailure,
    manyTill,
    takeWhile1P,
  )
import Text.Megaparsec.Char (char, eol)
import Text.Megaparsec.Char.Lexer (charLiteral)
import Prelude hiding (head)

-- Identifier
newtype Identifier = Identifier {toText :: Text}
  deriving (Eq, Ord, Show)

parseIdentifier :: MedeaParser Identifier
parseIdentifier = do
  ident <- takeWhile1P (Just "Non-separator") (not . isSeparatorOrControl)
  checkedConstruct Identifier ident

data ReservedIdentifier
  = RSchema
  | RStart
  | RType
  | RStringValues
  | RProperties
  | RPropertyName
  | RPropertySchema
  | RAdditionalPropertiesAllowed
  | ROptionalProperty
  | RMinLength
  | RMaxLength
  | RElementType
  | RTuple
  | RArray
  | RBoolean
  | RNull
  | RNumber
  | RObject
  | RString
  deriving (Eq, Show)

fromReserved :: ReservedIdentifier -> Text
fromReserved RSchema = "$schema"
fromReserved RStart = "$start"
fromReserved RType = "$type"
fromReserved RStringValues = "$string-values"
fromReserved RProperties = "$properties"
fromReserved RPropertyName = "$property-name"
fromReserved RPropertySchema = "$property-schema"
fromReserved RAdditionalPropertiesAllowed = "$additional-properties-allowed"
fromReserved ROptionalProperty = "$optional-property"
fromReserved RMinLength = "$min-length"
fromReserved RMaxLength = "$max-length"
fromReserved RElementType = "$element-type"
fromReserved RTuple = "$tuple"
fromReserved RArray = "$array"
fromReserved RBoolean = "$boolean"
fromReserved RNull = "$null"
fromReserved RNumber = "$number"
fromReserved RObject = "$object"
fromReserved RString = "$string"

identFromReserved :: ReservedIdentifier -> Identifier
identFromReserved = Identifier . fromReserved

tryReserved :: Text -> Maybe ReservedIdentifier
tryReserved "$schema" = Just RSchema
tryReserved "$start" = Just RStart
tryReserved "$type" = Just RType
tryReserved "$string-values" = Just RStringValues
tryReserved "$properties" = Just RProperties
tryReserved "$property-name" = Just RPropertyName
tryReserved "$property-schema" = Just RPropertySchema
tryReserved "$additional-properties-allowed" = Just RAdditionalPropertiesAllowed
tryReserved "$optional-property" = Just ROptionalProperty
tryReserved "$min-length" = Just RMinLength
tryReserved "$max-length" = Just RMaxLength
tryReserved "$element-type" = Just RElementType
tryReserved "$tuple" = Just RTuple
tryReserved "$array" = Just RArray
tryReserved "$boolean" = Just RBoolean
tryReserved "$null" = Just RNull
tryReserved "$number" = Just RNumber
tryReserved "$object" = Just RObject
tryReserved "$string" = Just RString
tryReserved _ = Nothing

parseReserved :: ReservedIdentifier -> MedeaParser Identifier
parseReserved reserved = do
  ident <- takeWhile1P Nothing (not . isSeparatorOrControl)
  let reservedText = fromReserved reserved
  when (ident /= reservedText) $ customFailure . ExpectedReservedIdentifier $ reservedText
  checkedConstruct Identifier ident

newtype PrimTypeIdentifier = PrimTypeIdentifier {typeOf :: JSONType}
  deriving (Eq)

tryPrimType :: Identifier -> Maybe PrimTypeIdentifier
tryPrimType (Identifier ident) = tryReserved ident >>= reservedToPrim

reservedToPrim :: ReservedIdentifier -> Maybe PrimTypeIdentifier
reservedToPrim RNull = Just . PrimTypeIdentifier $ JSONNull
reservedToPrim RBoolean = Just . PrimTypeIdentifier $ JSONBoolean
reservedToPrim RObject = Just . PrimTypeIdentifier $ JSONObject
reservedToPrim RArray = Just . PrimTypeIdentifier $ JSONArray
reservedToPrim RNumber = Just . PrimTypeIdentifier $ JSONNumber
reservedToPrim RString = Just . PrimTypeIdentifier $ JSONString
reservedToPrim _ = Nothing

isReserved :: Identifier -> Bool
isReserved = isJust . tryReserved . toText

isStartIdent :: Identifier -> Bool
isStartIdent = (== Just RStart) . tryReserved . toText

-- Natural Number
type Natural = Word

parseNatural :: MedeaParser Natural
parseNatural = do
  digits <- takeWhile1P (Just "digits") isDigit
  when (head digits == '0')
    $ customFailure . LeadingZero
    $ digits
  pure . read . unpack $ digits

-- String
newtype MedeaString = MedeaString {unwrap :: Text}
  deriving (Eq, Ord, Show, Hashable)

parseString :: MedeaParser MedeaString
parseString = do
  string <- char '"' *> manyTill charLiteral (char '"')
  pure . MedeaString . pack $ string

{-# INLINE parseLine #-}
parseLine :: Int -> MedeaParser a -> MedeaParser a
parseLine spaces p = replicateM_ spaces (char ' ') *> p <* eol

parseKeyVal :: ReservedIdentifier -> MedeaParser a -> MedeaParser a
parseKeyVal key = (parseReserved key *> char ' ' *>)

-- Helpers
checkedConstruct ::
  (Text -> a) -> Text -> MedeaParser a
checkedConstruct f t =
  if (> 32) . BS.length . encodeUtf8 $ t
    then customFailure . IdentifierTooLong $ t
    else pure . f $ t

isSeparatorOrControl :: Char -> Bool
isSeparatorOrControl c = isSeparator c || isControl c

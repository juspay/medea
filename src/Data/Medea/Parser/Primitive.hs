{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Medea.Parser.Primitive where

import Control.Monad (replicateM_, when)
import qualified Data.ByteString as BS
import Data.Char (isControl, isDigit, isSeparator)
import Data.Functor (($>))
import Data.Hashable (Hashable (..))
import Data.Maybe (isJust)
import Data.Medea.JSONType (JSONType (..))
import Data.Medea.Parser.Types (MedeaParser, ParseError (..))
import Data.Text (Text, cons, head, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Text.Megaparsec
  ( (<|>),
    chunk,
    customFailure,
    manyTill,
    single,
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

tryReserved :: Identifier -> Maybe ReservedIdentifier
tryReserved (Identifier ident) =
  if head ident == '$'
    then Just (ReservedIdentifier ident)
    else Nothing

forgetReserved :: ReservedIdentifier -> Identifier
forgetReserved (ReservedIdentifier ident) = Identifier ident

newtype PrimTypeIdentifier = PrimTypeIdentifier {typeOf :: JSONType}
  deriving (Eq)

parsePrimType :: MedeaParser PrimTypeIdentifier
parsePrimType =
  PrimTypeIdentifier
    <$> ( chunk "$null" $> JSONNull
            <|> chunk "$boolean" $> JSONBoolean
            <|> chunk "$object" $> JSONObject
            <|> chunk "$array" $> JSONArray
            <|> chunk "$number" $> JSONNumber
            <|> chunk "$string" $> JSONString
        )

tryPrimType :: Identifier -> Maybe PrimTypeIdentifier
tryPrimType (Identifier ident) =
  PrimTypeIdentifier
    <$> ( case ident of
            "$null" -> Just JSONNull
            "$boolean" -> Just JSONBoolean
            "$object" -> Just JSONObject
            "$array" -> Just JSONArray
            "$number" -> Just JSONNumber
            "$string" -> Just JSONString
            _ -> Nothing
        )

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
  checkedConstruct ReservedIdentifier ident

{-# INLINE parseLine #-}
parseLine :: Int -> MedeaParser a -> MedeaParser a
parseLine spaces p = replicateM_ spaces (char ' ') *> p <* eol

isSeparatorOrControl :: Char -> Bool
isSeparatorOrControl c = isSeparator c || isControl c

parseKeyVal :: Text -> MedeaParser a -> MedeaParser a
parseKeyVal key = (parseReservedChunk key *> char ' ' *>)

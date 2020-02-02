{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Medea.Parsing where

import Data.Text.Short (ShortText, fromByteString)
import Data.ByteString (ByteString, pack)
import Data.Word (Word8)
import Streamly (IsStream, MonadAsync)
import Streamly.Prelude (Enumerable(..), splitOnSuffix)
import Control.Monad.Except (MonadError(..))

import qualified Data.ByteString as BS
import qualified Streamly.Prelude as SP
import qualified Streamly.Data.Fold as SDF

data ParseError = 
  -- Invalid amount of spaces
  InvalidSpaces Int -- How many spaces
                LineNumber | -- Where
  -- Schema header wasn't well-formed
  InvalidSchemaHeader LineNumber | -- where
  -- We got something which isn't UTF-8
  NotUTF8 LineNumber | -- where
  -- Should have a $schema tag on a schema header, but didn't
  MissingSchemaTag LineNumber | -- where
  -- Should have a $type tag on a type specifier, but didn't
  MissingTypeTag LineNumber -- where

-- Parser innards

newtype LineNumber = LineNumber { expose :: Word }
  deriving (Eq, Ord, Bounded)

-- gah
instance Enum LineNumber where
  toEnum = LineNumber . toEnum
  fromEnum = fromEnum . expose

instance Enumerable LineNumber where
  enumerateFrom = fmap LineNumber . enumerateFrom . expose
  enumerateFromTo (LineNumber n) = fmap LineNumber . enumerateFromTo n . expose
  enumerateFromThen (LineNumber n) = fmap LineNumber . enumerateFromThen n . expose
  enumerateFromThenTo (LineNumber from) (LineNumber n) = fmap LineNumber . enumerateFromThenTo from n . expose

data LineToken =
  -- A '$schema' line starting a schema def
  SchemaHeader ShortText |
  -- $type - start of a type spec section
  TypeSpecHeader |
  -- A single type specifier
  TypeSpecifier ShortText

data Line = Line { 
  line :: LineNumber,
  tok :: LineToken
}

makeLine :: (MonadError ParseError m) => 
  LineNumber -> ByteString -> m Line
makeLine lineNum bs = do
  let (lead, rest) = BS.span (32 ==) bs
  case BS.length lead of
    0 -> makeSchemaHeader lineNum rest
    4 -> makeTypeSpecHeader lineNum rest
    8 -> makeTypeSpecifier lineNum rest
    n -> throwError . InvalidSpaces n $ lineNum 

makeSchemaHeader :: (MonadError ParseError m) => 
  LineNumber -> ByteString -> m Line
makeSchemaHeader lineNum bs = do
  let pieces = BS.splitWith (32 ==) bs
  case pieces of
    [tag, identifier] -> do
      let encTag = fromByteString tag
      let encIdentifier = fromByteString identifier
      case (encTag, encIdentifier) of
        (Just t, Just i) -> if t == "$schema"
                            then pure . Line lineNum . SchemaHeader $ i
                            else throwError . MissingSchemaTag $ lineNum
        _ -> throwError . NotUTF8 $ lineNum 
    _ -> throwError . InvalidSchemaHeader $ lineNum

makeTypeSpecHeader :: (MonadError ParseError m) => 
  LineNumber -> ByteString -> m Line
makeTypeSpecHeader lineNum bs = case fromByteString bs of
    Just t -> if t == "$type"
              then pure . Line lineNum $ TypeSpecHeader
              else throwError . MissingTypeTag $ lineNum
    Nothing -> throwError . NotUTF8 $ lineNum

makeTypeSpecifier :: (MonadError ParseError m) => 
  LineNumber -> ByteString -> m Line
makeTypeSpecifier lineNum bs = case fromByteString bs of
    Just i -> pure . Line lineNum . TypeSpecifier $ i
    Nothing -> throwError . NotUTF8 $ lineNum

-- TODO: Currently, this only handles LF, not CRLF or w/e.
tagLineNum :: (Monad m, IsStream t) => 
  t m Word8 -> t m (LineNumber, ByteString)
tagLineNum = SP.zipWith (,) (enumerateFrom minBound) . splitOnSuffix (== 10) (pack <$> SDF.toList)

intoLines :: (IsStream t, MonadAsync m, MonadError ParseError m) => t m (LineNumber, ByteString) -> t m Line
intoLines = SP.mapM (uncurry makeLine)

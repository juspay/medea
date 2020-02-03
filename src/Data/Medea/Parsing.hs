{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Medea.Parsing where

import Data.Text.Short (ShortText, fromByteString)
import Data.ByteString (ByteString, pack)
import Data.Word (Word8)
import Streamly (IsStream, MonadAsync, SerialT)
import Streamly.Prelude (Enumerable(..), splitOnSuffix)
import Streamly.Internal.FileSystem.File (toBytes)
import Control.Monad.Except (MonadError(..), ExceptT, runExceptT)
import Data.Vector (Vector, unfoldrM)

import qualified Data.ByteString as BS
import qualified Streamly.Prelude as SP
import qualified Streamly.Data.Fold as SDF

import Data.Medea.Types.Lines (LineNumber(..), LineToken(..), Line(..))
import Data.Medea.Error (LoaderError(..), ParseError(..), throwParseError)

-- Parser innards

makeLine :: (MonadError LoaderError m) => 
  LineNumber -> ByteString -> m (Line ShortText)
makeLine lineNum bs = do
  let (lead, rest) = BS.span (32 ==) bs
  case BS.length lead of
    0 -> if BS.null rest
         then pure . Line lineNum $ NewLine
         else makeSchemaHeader lineNum rest
    4 -> makeTypeSpecHeader lineNum rest
    8 -> makeTypeSpecifier lineNum rest
    n -> throwParseError . InvalidSpaces n $ lineNum 

makeSchemaHeader :: (MonadError LoaderError m) => 
  LineNumber -> ByteString -> m (Line ShortText)
makeSchemaHeader lineNum bs = do
  let pieces = BS.splitWith (32 ==) bs
  case pieces of
    [tag, identifier] -> do
      let encTag = fromByteString tag
      let encIdentifier = fromByteString identifier
      case (encTag, encIdentifier) of
        (Just t, Just i) -> if t == "$schema"
                            then pure . Line lineNum . SchemaHeader $ i
                            else throwParseError . MissingSchemaTag $ lineNum
        _ -> throwParseError . NotUTF8 $ lineNum 
    _ -> throwParseError . InvalidSchemaHeader $ lineNum

makeTypeSpecHeader :: (MonadError LoaderError m) => 
  LineNumber -> ByteString -> m (Line ShortText)
makeTypeSpecHeader lineNum bs = case fromByteString bs of
    Just t -> if t == "$type"
              then pure . Line lineNum $ TypeSpecHeader
              else throwParseError . MissingTypeTag $ lineNum
    Nothing -> throwParseError . NotUTF8 $ lineNum

makeTypeSpecifier :: (MonadError LoaderError m) => 
  LineNumber -> ByteString -> m (Line ShortText)
makeTypeSpecifier lineNum bs = case fromByteString bs of
    Just i -> pure . Line lineNum . TypeSpecifier $ i
    Nothing -> throwParseError . NotUTF8 $ lineNum

-- TODO: Currently, this only handles LF, not CRLF or w/e.
tagLineNum :: (Monad m, IsStream t) => 
  t m Word8 -> t m (LineNumber, ByteString)
tagLineNum = SP.zipWith (,) (enumerateFrom minBound) . splitOnSuffix (== 10) (pack <$> SDF.toList)

intoLines :: (IsStream t, MonadAsync m, MonadError LoaderError m) => t m (LineNumber, ByteString) -> t m (Line ShortText)
intoLines = SP.mapM (uncurry makeLine)

parse :: FilePath -> IO (Either LoaderError (Vector (Line ShortText)))
parse fp = do
  let strm = intoLines @SerialT @(ExceptT LoaderError IO) . tagLineNum . toBytes $ fp
  result <- runExceptT . SP.null $ strm
  case result of
    (Left e) -> pure . Left $ e
    Right isEmpty -> if isEmpty
                     then pure (Left . ParseFailed $ NoSchemata)
                     else runExceptT . unfoldrM SP.uncons $ strm

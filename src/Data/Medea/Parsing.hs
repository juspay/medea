{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Data.Medea.Parsing where

import Data.Text.Short (ShortText, pack)
import Control.Monad.State.Strict (MonadState(..), modify)
import Control.Monad.Except (MonadError(..))
import Data.Word (Word8)
import Streamly.Internal.Data.Unicode.Stream (decodeUtf8Either, lines)
import Streamly (IsStream, MonadAsync)
import Prelude hiding (lines, words)

import qualified Streamly.Prelude as SP
import qualified Streamly.Data.Fold as SDF

data ParseError = 
  -- got some non-UTF8 encoding
  NotUTF8

-- Parser innards

data LineToken =
  -- First '$schemata' line 
  SchemataHeader |
  -- Identifier in schemata section
  SchemaDeclaration ShortText |
  -- A '$schema' line starting a schema def
  SchemaHeader ShortText |
  -- $type - start of a type spec section
  TypeSpecHeader |
  -- A single type specifier
  TypeSpecifier ShortText

resolveUtf8Encoding :: (MonadAsync m, MonadError ParseError m, IsStream t) => 
  t m Word8 -> t m Char
resolveUtf8Encoding = SP.mapM go . decodeUtf8Either
  where go = \case Left _ -> throwError NotUTF8
                   Right c -> pure c

tagLines :: (MonadAsync m, IsStream t, MonadState Word m) => 
  t m Char -> t m (Word, ShortText)
tagLines = SP.mapM go . lines (pack <$> SDF.toList)
  where go w = do
          line <- get
          modify (+ 1)
          pure (line, w)


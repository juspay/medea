{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Medea.Parser.Spec.Type where

import Data.Text (Text)
import Control.Monad (replicateM_)
import Text.Megaparsec (MonadParsec(..), some)
import Text.Megaparsec.Char (eol, char)
import Data.Vector (Vector)   

import qualified Data.Vector as V

import Data.Medea.Parser.Primitive (Identifier, 
                                     parseIdentifier, parseTypeHeader)
import Data.Medea.Parser.Types (MedeaParser, ParseError)

newtype Specification = Specification (Vector Identifier)
  deriving (Eq)

getReferences :: Specification -> [Identifier]
getReferences (Specification v) = V.toList v

parseSpecification :: MedeaParser Specification
parseSpecification = do
  replicateM_ 4 $ char ' '
  _ <- parseTypeHeader
  _ <- eol
  Specification . V.fromList <$> some (try parseTypeSpec)
  where parseTypeSpec = do
          replicateM_ 8 $ char ' '
          ident <- parseIdentifier
          _ <- eol
          pure ident
  

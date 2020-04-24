{-# LANGUAGE FlexibleContexts #-}

module Data.Medea.Parser.Spec.Type
  ( Specification (..),
    defaultSpec,
    parseSpecification,
  )
where

import Data.Medea.Parser.Primitive
  ( Identifier,
    ReservedIdentifier(..),
    parseIdentifier,
    parseLine,
    parseReserved,
  )
import Data.Medea.Parser.Types (MedeaParser)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.Megaparsec (MonadParsec (..), some)

newtype Specification = Specification (Vector Identifier)
  deriving (Eq)

defaultSpec :: Specification
defaultSpec = Specification V.empty

parseSpecification :: MedeaParser Specification
parseSpecification = do
  _ <- parseLine 4 $ parseReserved RType
  types <- some . try $ parseLine 8 parseIdentifier
  pure . Specification . V.fromList $ types

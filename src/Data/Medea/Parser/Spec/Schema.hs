{-# LANGUAGE FlexibleContexts #-}

module Data.Medea.Parser.Spec.Schema where 

import Control.Applicative.Permutations (runPermutation,
                                         toPermutation,
                                         toPermutationWithDefault)
import Data.Text (Text)
import Text.Megaparsec (MonadParsec(..), (<|>))
import Text.Megaparsec.Char (eol, char)

import Data.Medea.Parser.Types (MedeaParser, ParseError)
import Data.Medea.Parser.Primitive (Identifier, 
                                     parseIdentifier, parseSchemaHeader)

import qualified Data.Vector as V
import qualified Data.Medea.Parser.Spec.Array as Array
import qualified Data.Medea.Parser.Spec.Type as Type

data Specification = Specification {
  name :: !Identifier,
  types :: !Type.Specification,
  arrayDim :: !Array.Specification
}
  deriving (Eq)

parseSpecification :: MedeaParser Specification
parseSpecification = do
  _ <- parseSchemaHeader
  _ <- char ' '
  schemaName <- parseIdentifier
  _ <- eol
  runPermutation $ Specification schemaName
    <$> toPermutationWithDefault (Type.Specification V.empty) (try Type.parseSpecification)
    <*> toPermutationWithDefault Array.defaultSpec (try Array.parseSpecification)

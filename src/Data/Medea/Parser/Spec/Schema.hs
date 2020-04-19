{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Medea.Parser.Spec.Schema where 

import Control.Applicative.Permutations (runPermutation,
                                         toPermutationWithDefault)
import Text.Megaparsec (MonadParsec(..))

import Data.Medea.Parser.Types (MedeaParser)
import Data.Medea.Parser.Primitive (Identifier, parseKeyVal,
                                    parseIdentifier, parseLine)

import qualified Data.Medea.Parser.Spec.Array as Array
import qualified Data.Medea.Parser.Spec.Object as Object
import qualified Data.Medea.Parser.Spec.Type as Type
import qualified Data.Medea.Parser.Spec.String as String

data Specification = Specification {
  name :: !Identifier,
  types :: !Type.Specification,
  stringVals :: !String.Specification,
  array :: !Array.Specification,
  object :: !Object.Specification
}
  deriving (Eq)

parseSpecification :: MedeaParser Specification
parseSpecification = do
  schemaName <- parseLine 0 $ parseKeyVal "schema" parseIdentifier
  runPermutation $ Specification schemaName
    <$> toPermutationWithDefault Type.defaultSpec (try Type.parseSpecification)
    <*> toPermutationWithDefault String.defaultSpec (try String.parseSpecification)
    <*> toPermutationWithDefault Array.defaultSpec (try Array.parseSpecification)
    <*> toPermutationWithDefault Object.defaultSpec (try Object.parseSpecification)

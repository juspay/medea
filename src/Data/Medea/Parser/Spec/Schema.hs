{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Medea.Parser.Spec.Schema
  ( Specification (..),
    parseSpecification,
  )
where

import Control.Applicative.Permutations
  ( runPermutation,
    toPermutationWithDefault,
  )
import Data.Medea.Parser.Primitive
  ( Identifier,
    ReservedIdentifier (..),
    parseIdentifier,
    parseKeyVal,
    parseLine,
  )
import qualified Data.Medea.Parser.Spec.Array as Array
import qualified Data.Medea.Parser.Spec.Object as Object
import qualified Data.Medea.Parser.Spec.String as String
import qualified Data.Medea.Parser.Spec.Type as Type
import Data.Medea.Parser.Types (MedeaParser)
import Text.Megaparsec (MonadParsec (..))

data Specification = Specification
  { name :: !Identifier,
    types :: !Type.Specification,
    stringVals :: !String.Specification,
    array :: !Array.Specification,
    object :: !(Maybe Object.Specification)
  }
  deriving stock (Eq)

parseSpecification :: MedeaParser Specification
parseSpecification = do
  schemaName <- parseLine 0 $ parseKeyVal RSchema parseIdentifier
  runPermutation $
    Specification schemaName
      <$> toPermutationWithDefault Type.defaultSpec (try Type.parseSpecification)
      <*> toPermutationWithDefault String.defaultSpec (try String.parseSpecification)
      <*> toPermutationWithDefault Array.defaultSpec (try Array.parseSpecification)
      <*> toPermutationWithDefault Nothing (Just <$> try Object.parseSpecification)

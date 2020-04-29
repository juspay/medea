{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TestM
  ( TestM (..),
    isParseError,
    isSchemaError,
    listMedeaFiles,
    runTestM,
  )
where

import Control.Monad.Except (ExceptT, MonadError, runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Data.List (sort)
import Data.Medea (LoaderError (..))
import System.Directory (listDirectory)
import System.FilePath ((</>), isExtensionOf)

newtype TestM a = TestM (ExceptT LoaderError IO a)
  deriving (Functor, Applicative, Monad, MonadError LoaderError, MonadIO)

runTestM :: TestM a -> IO (Either LoaderError a)
runTestM (TestM comp) = runExceptT comp

isParseError :: Either LoaderError a -> Bool
isParseError (Left NotUtf8) = True
isParseError (Left IdentifierTooLong) = True
isParseError (Left (ParserError _)) = True
isParseError _ = False

isSchemaError :: Either LoaderError a -> Bool
isSchemaError (Left StartSchemaMissing) = True
isSchemaError (Left SelfTypingSchema) = True
isSchemaError (Left (MultipleSchemaDefinition _)) = True
isSchemaError (Left (MissingSchemaDefinition _ _)) = True
isSchemaError (Left (SchemaNameReserved _)) = True
isSchemaError (Left (IsolatedSchemata _)) = True
isSchemaError (Left (MissingPropSchemaDefinition _ _)) = True
isSchemaError (Left (MinimumLengthGreaterThanMaximum _)) = True
isSchemaError (Left (MultiplePropSchemaDefinition _ _)) = True
isSchemaError (Left (MissingListSchemaDefinition _ _)) = True
isSchemaError (Left (MissingTupleSchemaDefinition _ _)) = True
isSchemaError (Left (PropertySpecWithoutObjectType _)) = True
isSchemaError (Left (ListSpecWithoutArrayType _)) = True
isSchemaError (Left (TupleSpecWithoutArrayType _)) = True
isSchemaError (Left (StringSpecWithoutStringType _)) = True
isSchemaError _ = False

listMedeaFiles :: FilePath -> IO [FilePath]
listMedeaFiles dir = fmap (dir </>) . sort . filter (isExtensionOf ".medea") <$> listDirectory dir

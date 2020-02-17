{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TestM where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Except (MonadError, ExceptT, runExceptT)

import Data.Medea.Loader (LoaderError(..))

newtype TestM a = TestM (ExceptT LoaderError IO a)
  deriving (Functor, Applicative, Monad, MonadError LoaderError, MonadIO)

runTestM :: TestM a -> IO (Either LoaderError a)
runTestM (TestM comp) = runExceptT comp

isParseError :: Either LoaderError a -> Bool
isParseError (Left NotUtf8) = True
isParseError (Left IdentifierTooLong) = True
isParseError (Left (ParserError _)) = True
isParseError _  = False

-- TODO: Fill in properly
isSchemaError :: Either LoaderError a -> Bool
isSchemaError (Left StartSchemaMissing) = True
isSchemaError (Left SelfTypingSchema) = True
isSchemaError (Left (MultipleSchemaDefinition _)) = True
isSchemaError (Left (MissingSchemaDefinition _)) = True
isSchemaError _ = False

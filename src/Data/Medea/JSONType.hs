{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module Data.Medea.JSONType
  ( JSONType (..),
    typeOf,
  )
where

import Data.Aeson (Value (..))
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

-- | The basic types of JSON value (as per
-- [ECMA-404](http://www.ecma-international.org/publications/files/ECMA-ST/ECMA-404.pdf)).
data JSONType
  = JSONNull
  | JSONBoolean
  | JSONNumber
  | JSONString
  | JSONArray
  | JSONObject
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable)

-- | Helper for determining the type of an Aeson 'Value'.
typeOf :: Value -> JSONType
typeOf = \case
  Object _ -> JSONObject
  Array _ -> JSONArray
  String _ -> JSONString
  Number _ -> JSONNumber
  Bool _ -> JSONBoolean
  Null -> JSONNull

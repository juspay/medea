{-# LANGUAGE LambdaCase #-}

module Data.Medea.JSONType
  ( JSONType (..),
    typeOf,
  )
where

import Data.Aeson (Value (..))

data JSONType
  = JSONNull
  | JSONBoolean
  | JSONNumber
  | JSONString
  | JSONArray
  | JSONObject
  deriving (Eq, Ord, Show)

typeOf :: Value -> JSONType
typeOf = \case
  Object _ -> JSONObject
  Array _ -> JSONArray
  String _ -> JSONString
  Number _ -> JSONNumber
  Bool _ -> JSONBoolean
  Null -> JSONNull

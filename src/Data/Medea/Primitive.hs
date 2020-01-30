{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Data.Medea.Primitive where

import Data.Text.Short (ShortText)
import Data.Aeson (Value(..))

data Primitive = 
  NullT |
  BooleanT |
  ObjectT |
  ArrayT |
  NumberT |
  StringT
  deriving (Eq)

typeOf :: Value -> Primitive
typeOf = \case
  Null -> NullT
  Bool _ -> BooleanT
  Object _ -> ObjectT
  Number _ -> NumberT
  String _ -> StringT
  Array _ -> ArrayT

nameOf :: Primitive -> ShortText
nameOf = \case
  NullT -> "$null"
  BooleanT -> "$boolean"
  ObjectT -> "$object"
  ArrayT -> "$array"
  NumberT -> "$number"
  StringT -> "$string"

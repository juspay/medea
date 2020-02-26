{-# LANGUAGE DeriveFunctor #-}

module Data.Medea.ValidJSON where

import Control.Comonad.Cofree (Cofree)
import Data.Text (Text)
import Data.Aeson (Value)
import Data.Vector (Vector)
import Data.HashMap.Strict (HashMap)

data ValidLeaf b = ValidLeaf b Value

data ValidInternal b a = 
  ValidArray b (Vector a) |
  ValidObject b (HashMap Text a)
  deriving (Functor)

newtype ValidJSON b = ValidJSON (Cofree (ValidInternal b) (ValidLeaf b))

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Medea.Identifier where

import Data.Hashable (Hashable)
import Data.Text.Short (ShortText)

newtype Identifier = Identifier ShortText
  deriving (Eq, Hashable)

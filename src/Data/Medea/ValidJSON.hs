{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Medea.ValidJSON where

import Data.Vector.Instances()
import Data.Hashable (Hashable(..))
import Data.Coerce (coerce)
import Control.DeepSeq (NFData(..))
import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.Functor.Classes (Eq1(..), Show1(..))
import Control.Comonad.Cofree (Cofree(..))
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Aeson (Value(..))
import Data.Vector (Vector)
import Data.HashMap.Strict (HashMap)

data ValidJSONF a = 
  AnythingF !Value |
  NullF |
  BooleanF {-# UNPACK #-} !Bool |
  NumberF {-# UNPACK #-} !Scientific |
  StringF !Text |
  ArrayF !(Vector a) |
  ObjectF !(HashMap Text a)
  deriving (Functor, Typeable, Data)

instance (NFData a) => NFData (ValidJSONF a) where
  {-# INLINE rnf #-}
  rnf (AnythingF v) = rnf v
  rnf NullF = ()
  rnf (BooleanF b) = rnf b
  rnf (NumberF n) = rnf n
  rnf (StringF s) = rnf s
  rnf (ArrayF v) = rnf v
  rnf (ObjectF hm) = rnf hm

instance Eq1 ValidJSONF where
  {-# INLINE liftEq #-}
  liftEq _ (AnythingF v) (AnythingF v') = v == v'
  liftEq _ NullF NullF = True
  liftEq _ (BooleanF b) (BooleanF b') = b == b'
  liftEq _ (NumberF n) (NumberF n') = n == n'
  liftEq _ (StringF s) (StringF s') = s == s'
  liftEq f (ArrayF v) (ArrayF v') = liftEq f v v'
  liftEq f (ObjectF hm) (ObjectF hm') = liftEq f hm hm'
  liftEq _ _ _ = False

instance Show1 ValidJSONF where
  liftShowsPrec _ _ prec (AnythingF v) = showsPrec prec v
  liftShowsPrec _ _ prec NullF = showsPrec prec Null
  liftShowsPrec _ _ prec (BooleanF b) = showsPrec prec  b
  liftShowsPrec _ _ prec (NumberF n) = showsPrec prec n
  liftShowsPrec _ _ prec (StringF s) = showsPrec prec s
  liftShowsPrec f g prec (ArrayF v) = liftShowsPrec f g prec v
  liftShowsPrec f g prec (ObjectF hm) = liftShowsPrec f g prec hm

instance (Hashable a) => Hashable (ValidJSONF a) where
  {-# INLINE hashWithSalt #-}
  hashWithSalt salt (AnythingF v) = hashWithSalt salt v
  hashWithSalt salt NullF = hashWithSalt salt Null
  hashWithSalt salt (BooleanF b) = hashWithSalt salt b
  hashWithSalt salt (NumberF n) = hashWithSalt salt n
  hashWithSalt salt (StringF s) = hashWithSalt salt s
  hashWithSalt salt (ArrayF v) = hashWithSalt salt v
  hashWithSalt salt (ObjectF hm) = hashWithSalt salt hm

newtype ValidJSON = ValidJSON (Cofree ValidJSONF (Maybe Text))
  deriving (Eq, Data, Show)

-- Can't coerce-erase the constructor fmap, sigh
instance NFData ValidJSON where
  {-# INLINE rnf #-}
  rnf (ValidJSON (x :< f)) = rnf x `seq` (rnf . fmap ValidJSON $ f)

-- Nor here
instance Hashable ValidJSON where
  {-# INLINE hashWithSalt #-}
  hashWithSalt salt (ValidJSON (x :< f)) = salt `hashWithSalt` x `hashWithSalt` fmap ValidJSON f

-- | Convert to an Aeson representation (throwing away all schema information).
toValue :: ValidJSON -> Value
toValue (ValidJSON (_ :< f)) = case f of
  AnythingF v -> v
  NullF -> Null
  BooleanF b -> Bool b
  NumberF n -> Number n
  StringF s -> String s
  ArrayF v -> Array . fmap (toValue . coerce) $ v
  ObjectF hm -> Object . fmap (toValue . coerce) $ hm

-- | Get the name of the schema that this validated against.
validAgainst :: ValidJSON -> Text
validAgainst (ValidJSON (label :< f)) = case label of
  Nothing -> case f of
    AnythingF _ -> "$any"
    NullF -> "$null"
    BooleanF _ -> "$boolean"
    NumberF _ -> "$number"
    StringF _ -> "$string"
    ArrayF _ -> "$array"
    ObjectF _ -> "$object"
  Just scm -> scm

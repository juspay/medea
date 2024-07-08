{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}

module Data.Medea.ValidJSON (ValidJSONF (..)) where

import Control.DeepSeq (NFData (..))
import Data.Aeson (Value (..))
import Data.Data (Data)
import Data.Functor.Classes (Eq1 (..), Show1 (..))
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable (..))
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Vector (Vector)
import Data.Vector.Instances ()

data ValidJSONF a
  = AnythingF !Value
  | NullF
  | BooleanF !Bool
  | NumberF {-# UNPACK #-} !Scientific
  | StringF {-# UNPACK #-} !Text
  | ArrayF {-# UNPACK #-} !(Vector a)
  | ObjectF !(HashMap Text a)
  deriving stock (Functor, Typeable, Data, Eq)

instance Foldable ValidJSONF where
  {-# INLINE foldMap #-}
  foldMap _ (AnythingF _) = mempty
  foldMap _ NullF = mempty
  foldMap _ (BooleanF _) = mempty
  foldMap _ (NumberF _) = mempty
  foldMap _ (StringF _) = mempty
  foldMap f (ArrayF v) = foldMap f v
  foldMap f (ObjectF hm) = foldMap f hm

instance Traversable ValidJSONF where
  {-# INLINE traverse #-}
  traverse _ (AnythingF v) = pure . AnythingF $ v
  traverse _ NullF = pure NullF
  traverse _ (BooleanF b) = pure . BooleanF $ b
  traverse _ (NumberF n) = pure . NumberF $ n
  traverse _ (StringF s) = pure . StringF $ s
  traverse f (ArrayF v) = ArrayF <$> traverse f v
  traverse f (ObjectF hm) = ObjectF <$> traverse f hm

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
  liftShowsPrec _ _ prec (BooleanF b) = showsPrec prec b
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

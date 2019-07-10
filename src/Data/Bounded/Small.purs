module Data.Bounded.Small where

import Prelude (pure, (+), (-), ($), (<$>))

import Data.Bounded (class Bounded)
import Data.Enum (class BoundedEnum, Cardinality(..), cardinality, fromEnum, toEnum)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)

-- | A lawful subclass  of `Bounded `to denote
-- | Cardinality a << Cardinality Int
class Bounded a <= SmallBounded a

-- | A lawful subclass of `BoundedEnum` to denote
-- | Cardinality a << Cardinality Int
class BoundedEnum a <= SmallBoundedEnum a

-- instance smallBoundedEnumMaybe :: (SmallBounded a, BoundedEnum a)
--   => SmallBoundedEnum (Maybe a) where
--     cardinality = Cardinality $ unwrap (cardinality :: Cardinality a) + 1
--     toEnum 0 = Nothing
--     toEnum n = Just <$> toEnum (n - 1)
--     fromEnum Nothing = 0
--     fromEnum (Just e) = fromEnum e + 1

newtype MaybeWrapped a = MaybeWrapped (Maybe a)
derive instance newtypeMaybeWrapped :: Newtype (MaybeWrapped a) _

instance boundedEnumMaybe :: (SmallBounded a, BoundedEnum a)
  => BoundedEnum (MaybeWrapped a) where
    cardinality = Cardinality $ unwrap (cardinality :: Cardinality a) + 1
    toEnum 0 = wrap (Nothing :: Maybe a)
    toEnum n = wrap $ Just <$> toEnum (n - 1)
    fromEnum (MaybeWrapped Nothing) = 0
    fromEnum (MaybeWrapped (Just e)) = fromEnum e + 1


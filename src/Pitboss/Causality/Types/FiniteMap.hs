{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Pitboss.Causality.Types.FiniteMap where

import Control.Lens (At (..), Index, IxValue, Ixed (..))
import Data.Aeson
import Data.Map.Strict qualified as Map
import GHC.Generics (Generic)

newtype FiniteMap k v = FiniteMap (Map.Map k v)
    deriving (Eq, Show, Functor)

instance (ToJSON k, ToJSON v, Ord k, ToJSONKey k) => ToJSON (FiniteMap k v) where
    toJSON (FiniteMap m) = toJSON m

instance (FromJSON k, FromJSON v, Ord k, FromJSONKey k) => FromJSON (FiniteMap k v) where
    parseJSON v = FiniteMap <$> parseJSON v

instance (BoundedEnum k, Ord k, Semigroup v, Monoid v) => Monoid (FiniteMap k v) where
    mempty = emptyFiniteMap mempty

instance (BoundedEnum k, Ord k, Semigroup v) => Semigroup (FiniteMap k v) where
    FiniteMap a <> FiniteMap b =
        FiniteMap (Map.fromList [(k, a Map.! k <> b Map.! k) | k <- universe])

type instance Index (FiniteMap k v) = k

type instance IxValue (FiniteMap k v) = v

instance (Ord k) => Ixed (FiniteMap k v) where
    ix k f (FiniteMap m) = fmap FiniteMap (ix k f m)

instance (Ord k) => At (FiniteMap k v) where
    at k f (FiniteMap m) = fmap FiniteMap (at k f m)

deriving instance Foldable (FiniteMap k)

deriving instance Traversable (FiniteMap k)

emptyFiniteMap :: (BoundedEnum k, Ord k) => v -> FiniteMap k v
emptyFiniteMap defaultValue =
    FiniteMap (Map.fromList [(k, defaultValue) | k <- universe])

insertFiniteMap :: (Ord k) => k -> v -> FiniteMap k v -> FiniteMap k v
insertFiniteMap k v (FiniteMap m) = FiniteMap (Map.insert k v m)

lookupFiniteMap :: (Ord k) => k -> FiniteMap k v -> Maybe v
lookupFiniteMap k (FiniteMap m) = Map.lookup k m

keysFiniteMap :: FiniteMap k v -> [k]
keysFiniteMap (FiniteMap m) = Map.keys m

toListFiniteMap :: FiniteMap k v -> [(k, v)]
toListFiniteMap (FiniteMap m) = Map.toList m

mapFiniteMapWithKey :: (k -> v -> v') -> FiniteMap k v -> FiniteMap k v'
mapFiniteMapWithKey f (FiniteMap m) = FiniteMap (Map.mapWithKey f m)

singletonFiniteMap :: (BoundedEnum k, Ord k) => k -> v -> v -> FiniteMap k v
singletonFiniteMap key val defaultVal = insertFiniteMap key val (emptyFiniteMap defaultVal)

class (Enum a, Bounded a) => BoundedEnum a

universe :: (BoundedEnum a) => [a]
universe = [minBound .. maxBound]

data Occupancy a
    = Absent
    | Present a
    deriving (Eq, Show, Functor, Generic)

instance (ToJSON a) => ToJSON (Occupancy a)
instance (FromJSON a) => FromJSON (Occupancy a)

instance Semigroup (Occupancy a) where
    Absent <> x = x
    x <> Absent = x
    Present a <> Present _ = Present a

instance Monoid (Occupancy a) where
    mempty = Absent

isPresent :: Occupancy a -> Bool
isPresent (Present _) = True
isPresent Absent = False

isAbsent :: Occupancy a -> Bool
isAbsent = not . isPresent

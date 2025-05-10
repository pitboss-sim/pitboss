{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Pitboss.Registry.State.Types.FiniteMap where

import Control.Lens (At (..), Index, IxValue, Ixed (..))
import Data.Map.Strict qualified as Map
import Pitboss.Registry.State.Types.BoundedEnum (BoundedEnum, universe)

newtype FiniteMap k v = FiniteMap (Map.Map k v)
  deriving (Eq, Show, Functor)

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

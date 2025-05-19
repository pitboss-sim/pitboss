{-# LANGUAGE DeriveGeneric #-}

module Pitboss.State.Types.FiniteMap.Occupancy (
    Occupancy (..),
    isPresent,
    isAbsent,
) where

import Data.Aeson
import GHC.Generics

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

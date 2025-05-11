{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.TableShoe.Delta where

import Data.Aeson
import GHC.Generics
import Pitboss.Trace.Entity
import Pitboss.Trace.Entity.Capabilities

-- This entity is static but needs to quack like an entity.

data TableShoeEntityDelta
    = Noop
    deriving (Eq, Show, Generic)

instance Incremental TableShoeEntityDelta where
    type Target TableShoeEntityDelta = TableShoeEntity

    applyDelta Noop e = e
    previewDelta Noop = Just
    describeDelta Noop _ = "Noop"

instance Reversible TableShoeEntityDelta where
    invert = Right

instance ToJSON TableShoeEntityDelta

instance FromJSON TableShoeEntityDelta

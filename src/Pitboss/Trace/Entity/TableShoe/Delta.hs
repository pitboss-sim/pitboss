{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.TableShoe.Delta where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Trace.Entity
import Pitboss.Trace.Entity.Capabilities

-- This entity is static but must typecheck as an entity delta.

data TableShoeEntityDelta
    = Noop
    deriving (Eq, Show, Generic)

instance ToJSON TableShoeEntityDelta

instance FromJSON TableShoeEntityDelta

instance Incremental TableShoeEntityDelta where
    type Target TableShoeEntityDelta = TableShoeEntity

    applyDelta Noop e = e

    previewDelta Noop = Just

    describeDelta Noop _ = "Noop (TableShoe is static)"

instance Reversible TableShoeEntityDelta where
    invert Noop = Right Noop

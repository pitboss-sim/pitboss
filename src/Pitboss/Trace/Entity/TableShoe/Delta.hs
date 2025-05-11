{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.TableShoe.Delta where

import Data.Aeson
import GHC.Generics
import Pitboss.Trace.Entity
import Pitboss.Trace.Entity.Capabilities

data TableShoeEntityDelta
  = Noop
  deriving (Eq, Show, Generic)

instance Incremental TableShoeEntityDelta where
  type Entity TableShoeEntityDelta = TableShoeEntity

  applyDelta _ entity = entity
  previewDelta _ = Just
  describeDelta _ _ = ""

instance ToJSON TableShoeEntityDelta

instance FromJSON TableShoeEntityDelta

{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.TableShoeCursor.Delta.Modes where

import Data.Aeson
import GHC.Generics
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.TableShoeCursor

data TableShoeCursorEntityModesDelta
  = Nothing
  deriving (Eq, Show, Generic)

instance Incremental TableShoeCursorEntityModesDelta where
  type Entity TableShoeCursorEntityModesDelta = TableShoeCursorEntityModes

  applyDelta _ _ = undefined
  previewDelta delta entity = Just $ applyDelta delta entity
  describeDelta _ _ = undefined

instance ToJSON TableShoeCursorEntityModesDelta where
  toJSON = undefined

instance FromJSON TableShoeCursorEntityModesDelta where
  parseJSON = undefined

instance Reversible TableShoeCursorEntityModesDelta where
  invert = undefined

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Delta.ShoeCursor where

import Data.Aeson
import GHC.Generics (Generic)
import Pitboss.Trace.Delta.Capabilities.Incremental
import Pitboss.Trace.Delta.Capabilities.Reversible
import Pitboss.Trace.Delta.ShoeCursor.Relation
import Pitboss.Trace.Delta.ShoeCursor.State
import Pitboss.Trace.Entity.ShoeCursor

data ShoeCursorDelta
  = ShoeCursorStateDelta ShoeCursorStateDelta
  | ShoeCursorRelationsDelta ShoeCursorRelationsDelta
  deriving (Eq, Show, Generic)

instance ToJSON ShoeCursorDelta

instance FromJSON ShoeCursorDelta

instance Incremental ShoeCursorDelta where
  type Entity ShoeCursorDelta = ShoeCursor

  applyDelta delta entity = case delta of
    ShoeCursorStateDelta d ->
      entity {_state = applyDelta d (_state entity)}
    ShoeCursorRelationsDelta d ->
      entity {_rels = applyDelta d (_rels entity)}

  previewDelta delta entity = Just $ applyDelta delta entity

  describeDelta delta entity = case delta of
    ShoeCursorStateDelta _ -> describeDelta delta entity
    ShoeCursorRelationsDelta (UpdateShoe old new) ->
      "Updated cursor shoe ref: " ++ show old ++ " → " ++ show new

instance Reversible ShoeCursorDelta where
  invert = \case
    ShoeCursorStateDelta d -> ShoeCursorStateDelta <$> invert d
    ShoeCursorRelationsDelta d -> ShoeCursorRelationsDelta <$> invert d

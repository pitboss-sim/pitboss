{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Pitboss.Trace.Entity.ShoeCursor.Delta
  ( module Pitboss.Trace.Entity.ShoeCursor.Delta.Attrs,
    module Pitboss.Trace.Entity.ShoeCursor.Delta.Modes,
    module Pitboss.Trace.Entity.ShoeCursor.Delta.Rels,
    ShoeCursorDelta (..),
  )
where

import Data.Aeson
import GHC.Generics (Generic)
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.ShoeCursor
import Pitboss.Trace.Entity.ShoeCursor.Delta.Attrs
import Pitboss.Trace.Entity.ShoeCursor.Delta.Modes
import Pitboss.Trace.Entity.ShoeCursor.Delta.Rels

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
      entity {_shoeCursorState = applyDelta d (_shoeCursorState entity)}
    ShoeCursorRelationsDelta d ->
      entity {_shoeCursorRels = applyDelta d (_shoeCursorRels entity)}

  previewDelta delta entity = Just $ applyDelta delta entity

  describeDelta delta entity = case delta of
    ShoeCursorStateDelta _ -> describeDelta delta entity
    ShoeCursorRelationsDelta (UpdateShoe old new) ->
      "Updated cursor shoe ref: " ++ show old ++ " → " ++ show new

instance Reversible ShoeCursorDelta where
  invert = \case
    ShoeCursorStateDelta d -> ShoeCursorStateDelta <$> invert d
    ShoeCursorRelationsDelta d -> ShoeCursorRelationsDelta <$> invert d

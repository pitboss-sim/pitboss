{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Delta.ShoeCursor.Relation where

import Data.Aeson
import GHC.Generics
import Pitboss.Trace.Delta.Capabilities.Incremental
import Pitboss.Trace.Delta.Capabilities.Reversible
import Pitboss.Trace.Entity.ShoeCursor
import Pitboss.Trace.Types.Identifier

data ShoeCursorRelationsDelta
  = UpdateShoe ShoeId ShoeId
  deriving (Eq, Show, Generic)

instance ToJSON ShoeCursorRelationsDelta

instance FromJSON ShoeCursorRelationsDelta

instance Incremental ShoeCursorRelationsDelta where
  type Entity ShoeCursorRelationsDelta = ShoeCursorRelations

  applyDelta delta rels = case delta of
    UpdateShoe _ new -> rels {_pointsToShoe = new}

  previewDelta delta entity = Just $ applyDelta delta entity

  describeDelta old new =
    "Updated cursor shoe ref: " ++ show old ++ " → " ++ show new

instance Reversible ShoeCursorRelationsDelta where
  invert = \case
    UpdateShoe old new -> Right (UpdateShoe new old)

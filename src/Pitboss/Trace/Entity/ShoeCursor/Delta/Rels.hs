{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.ShoeCursor.Delta.Rels where

import Data.Aeson
import GHC.Generics
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.ShoeCursor
import Pitboss.Trace.Types.EntityRef
import Pitboss.Trace.Types.Identifier

data ShoeCursorRelationsDelta
  = UpdateShoe (EntityRef ShoeId) (EntityRef ShoeId)
  deriving (Eq, Show, Generic)

instance ToJSON ShoeCursorRelationsDelta

instance FromJSON ShoeCursorRelationsDelta

instance Incremental ShoeCursorRelationsDelta where
  type Entity ShoeCursorRelationsDelta = ShoeCursorRelations

  applyDelta delta rels = case delta of
    UpdateShoe _ new -> rels {_shoeCursorRelsPointsToShoe = new}

  previewDelta delta entity = Just $ applyDelta delta entity

  describeDelta old new =
    "Updated cursor shoe ref: " ++ show old ++ " → " ++ show new

instance Reversible ShoeCursorRelationsDelta where
  invert = \case
    UpdateShoe old new -> Right (UpdateShoe new old)

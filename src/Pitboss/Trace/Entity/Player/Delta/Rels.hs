{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.Player.Delta.Rels where

import Data.Aeson
import GHC.Generics
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.Player
import Pitboss.Trace.Types.EntityRef
import Pitboss.Trace.Types.Identifier

data PlayerEntityRelsDelta
  = UpdateCloneOf (Maybe (EntityRef PlayerEntityId)) (Maybe (EntityRef PlayerEntityId))
  | UpdateSeatedAt (Maybe (EntityRef TableEntityId)) (Maybe (EntityRef TableEntityId))
  deriving (Eq, Show, Generic)

instance ToJSON PlayerEntityRelsDelta

instance FromJSON PlayerEntityRelsDelta

instance Incremental PlayerEntityRelsDelta where
  type Entity PlayerEntityRelsDelta = PlayerRelations

  applyDelta delta r = case delta of
    UpdateCloneOf _ new -> r {_playerRelsClonedFrom = new}
    UpdateSeatedAt _ new -> r {_playerRelsSeatedAt = new}

  previewDelta delta r = case delta of
    UpdateCloneOf old _ | _playerRelsClonedFrom r == old -> Just $ applyDelta delta r
    UpdateSeatedAt old _ | _playerRelsSeatedAt r == old -> Just $ applyDelta delta r
    _ -> Nothing

  describeDelta delta _ = case delta of
    UpdateCloneOf old new -> "Updated cloned-from: " ++ show old ++ " → " ++ show new
    UpdateSeatedAt old new -> "Updated table seat: " ++ show old ++ " → " ++ show new

instance Reversible PlayerEntityRelsDelta where
  invert = \case
    UpdateCloneOf old new -> Right (UpdateCloneOf new old)
    UpdateSeatedAt old new -> Right (UpdateSeatedAt new old)

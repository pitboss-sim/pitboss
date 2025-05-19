{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.Player.Delta.Rels where

import Data.Aeson
import GHC.Generics
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.Player.Types
import Pitboss.Trace.Entity.Types.EntityId

data PlayerEntityRelsDelta
    = UpdateCloneOf (Maybe (ClockedRef PlayerEntityId)) (Maybe (ClockedRef PlayerEntityId))
    | UpdateSeatedAt (Maybe (ClockedRef TableEntityId)) (Maybe (ClockedRef TableEntityId))
    deriving (Eq, Show, Generic)

instance ToJSON PlayerEntityRelsDelta
instance FromJSON PlayerEntityRelsDelta

instance Incremental PlayerEntityRelsDelta where
    type Target PlayerEntityRelsDelta = PlayerEntityRels

    applyDelta :: PlayerEntityRelsDelta -> PlayerEntityRels -> PlayerEntityRels
    applyDelta delta rels = case delta of
        UpdateCloneOf _ new -> rels{_playerEntityRelsClonedFrom = new}
        UpdateSeatedAt _ new -> rels{_playerEntityRelsSeatedAt = new}

    previewDelta :: PlayerEntityRelsDelta -> PlayerEntityRels -> Maybe PlayerEntityRels
    previewDelta delta rels = case delta of
        UpdateCloneOf old _ | _playerEntityRelsClonedFrom rels == old -> Just $ applyDelta delta rels
        UpdateSeatedAt old _ | _playerEntityRelsSeatedAt rels == old -> Just $ applyDelta delta rels
        _ -> Nothing

    describeDelta :: PlayerEntityRelsDelta -> PlayerEntityRels -> String
    describeDelta delta _ = case delta of
        UpdateCloneOf old new ->
            "Updated cloned-from: " ++ show old ++ " -> " ++ show new
        UpdateSeatedAt old new ->
            "Updated table seat: " ++ show old ++ " -> " ++ show new

instance Reversible PlayerEntityRelsDelta where
    invert = \case
        UpdateCloneOf old new -> Right (UpdateCloneOf new old)
        UpdateSeatedAt old new -> Right (UpdateSeatedAt new old)

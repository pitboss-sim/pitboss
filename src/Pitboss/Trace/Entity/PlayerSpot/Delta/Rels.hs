{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.PlayerSpot.Delta.Rels where

import Data.Aeson
import GHC.Generics
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.PlayerSpot.Types
import Pitboss.Trace.Entity.Types.FiniteMap
import Pitboss.Trace.Entity.Types.FiniteMap.Occupancy
import Pitboss.Trace.Entity.Types.EntityId

data PlayerSpotEntityRelsDelta
    = UpdatePlayer (ClockedRef PlayerEntityId) (ClockedRef PlayerEntityId)
    | UpdateRound (ClockedRef DealerRoundEntityId) (ClockedRef DealerRoundEntityId)
    | UpdateHandOccupancy
        (PlayerSpotHandIx, Occupancy (ClockedRef PlayerHandEntityId))
        (PlayerSpotHandIx, Occupancy (ClockedRef PlayerHandEntityId))
    deriving (Eq, Show, Generic)

instance ToJSON PlayerSpotEntityRelsDelta
instance FromJSON PlayerSpotEntityRelsDelta

instance Incremental PlayerSpotEntityRelsDelta where
    type Target PlayerSpotEntityRelsDelta = PlayerSpotEntityRels

    applyDelta delta rels = case delta of
        UpdatePlayer _ new -> rels{_playerSpotEntityRelsPlayerEntityId = new}
        UpdateRound _ new -> rels{_playerSpotEntityRelsRoundEntityId = new}
        UpdateHandOccupancy (_, _) (k, v) ->
            rels{_playerSpotEntityRelsHandOccupancy = insertFiniteMap k v (_playerSpotEntityRelsHandOccupancy rels)}

    previewDelta delta rels =
        case delta of
            UpdatePlayer old _
                | _playerSpotEntityRelsPlayerEntityId rels == old ->
                    Just (applyDelta delta rels)
            UpdateRound old _
                | _playerSpotEntityRelsRoundEntityId rels == old ->
                    Just (applyDelta delta rels)
            UpdateHandOccupancy (oldK, oldV) _
                | Just oldV == lookupFiniteMap oldK (_playerSpotEntityRelsHandOccupancy rels) ->
                    Just (applyDelta delta rels)
            _ -> Nothing

    describeDelta delta _ = case delta of
        UpdatePlayer old new ->
            "Updated player ID: " ++ show old ++ " -> " ++ show new
        UpdateRound old new ->
            "Updated round ID: " ++ show old ++ " -> " ++ show new
        UpdateHandOccupancy (k1, v1) (k2, v2) ->
            "Updated hand occupancy: " ++ show k1 ++ "=" ++ show v1 ++ " -> " ++ show k2 ++ "=" ++ show v2

instance Reversible PlayerSpotEntityRelsDelta where
    invert = \case
        UpdatePlayer old new -> Right (UpdatePlayer new old)
        UpdateRound old new -> Right (UpdateRound new old)
        UpdateHandOccupancy old new -> Right (UpdateHandOccupancy new old)

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.PlayerSpot.Delta.Rels where

import Data.Aeson
import GHC.Generics
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.PlayerSpot
import Pitboss.Trace.Entity.Types.FiniteMap
import Pitboss.Trace.Entity.Types.FiniteMap.Occupancy
import Pitboss.Trace.Types.EntityRef
import Pitboss.Trace.Types.Identifier

data PlayerSpotEntityRelsDelta
    = UpdatePlayer (EntityRef PlayerEntityId) (EntityRef PlayerEntityId)
    | UpdateRound (EntityRef DealerRoundEntityId) (EntityRef DealerRoundEntityId)
    | UpdateHandOccupancy (PlayerSpotHandIx, Occupancy (EntityRef PlayerHandEntityId)) (PlayerSpotHandIx, Occupancy (EntityRef PlayerHandEntityId))
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

    previewDelta delta rels = case delta of
        UpdatePlayer old _ ->
            if _playerSpotEntityRelsPlayerEntityId rels == old
                then Just $ applyDelta delta rels
                else Nothing
        UpdateRound old _ ->
            if _playerSpotEntityRelsRoundEntityId rels == old
                then Just $ applyDelta delta rels
                else Nothing
        UpdateHandOccupancy (oldK, oldV) _ ->
            case lookupFiniteMap oldK (_playerSpotEntityRelsHandOccupancy rels) of
                Just actualV
                    | actualV == oldV ->
                        Just $ applyDelta delta rels
                _ -> Nothing

    describeDelta delta _ = case delta of
        UpdatePlayer old new -> "Updated player ID: " ++ show old ++ " → " ++ show new
        UpdateRound old new -> "Updated round ID: " ++ show old ++ " → " ++ show new
        UpdateHandOccupancy (k1, v1) (k2, v2) ->
            "Updated hand occupancy: " ++ show k1 ++ "=" ++ show v1 ++ " → " ++ show k2 ++ "=" ++ show v2

instance Reversible PlayerSpotEntityRelsDelta where
    invert = \case
        UpdatePlayer old new -> Right (UpdatePlayer new old)
        UpdateRound old new -> Right (UpdateRound new old)
        UpdateHandOccupancy old new -> Right (UpdateHandOccupancy new old)

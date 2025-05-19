{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.PlayerHand.Delta.Rels where

import Data.Aeson
import GHC.Generics
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.PlayerHand.Types
import Pitboss.Trace.Types.EntityRef
import Pitboss.Trace.Types.Identifier

data PlayerHandEntityRelsDelta
    = UpdatePlayerSpot (EntityRef PlayerSpotEntityId) (EntityRef PlayerSpotEntityId)
    | UpdateDealerRound (EntityRef DealerRoundEntityId) (EntityRef DealerRoundEntityId)
    | UpdatePlayer (EntityRef PlayerEntityId) (EntityRef PlayerEntityId)
    deriving (Eq, Show, Generic)

instance ToJSON PlayerHandEntityRelsDelta
instance FromJSON PlayerHandEntityRelsDelta

instance Incremental PlayerHandEntityRelsDelta where
    type Target PlayerHandEntityRelsDelta = PlayerHandEntityRels

    applyDelta delta rels = case delta of
        UpdatePlayerSpot _ new -> rels{_playerHandEntityRelsBelongsToPlayerSpot = new}
        UpdateDealerRound _ new -> rels{_playerHandEntityRelsBelongsToDealerRound = new}
        UpdatePlayer _ new -> rels{_playerHandEntityRelsOwnedByPlayer = new}

    previewDelta delta rels = case delta of
        UpdatePlayerSpot old _ | old == _playerHandEntityRelsBelongsToPlayerSpot rels -> Just $ applyDelta delta rels
        UpdateDealerRound old _ | old == _playerHandEntityRelsBelongsToDealerRound rels -> Just $ applyDelta delta rels
        UpdatePlayer old _ | old == _playerHandEntityRelsOwnedByPlayer rels -> Just $ applyDelta delta rels
        _ -> Nothing

    describeDelta delta _ = case delta of
        UpdatePlayerSpot old new -> "Changed spot: " ++ show old ++ " -> " ++ show new
        UpdateDealerRound old new -> "Changed round: " ++ show old ++ " -> " ++ show new
        UpdatePlayer old new -> "Changed player: " ++ show old ++ " -> " ++ show new

instance Reversible PlayerHandEntityRelsDelta where
    invert = \case
        UpdatePlayerSpot old new -> Right (UpdatePlayerSpot new old)
        UpdateDealerRound old new -> Right (UpdateDealerRound new old)
        UpdatePlayer old new -> Right (UpdatePlayer new old)

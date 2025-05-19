{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.DealerHand.Delta.Rels where

import Data.Aeson
import GHC.Generics
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.DealerHand.Types
import Pitboss.Trace.Types.EntityRef
import Pitboss.Trace.Types.Identifier

data DealerHandEntityRelsDelta
    = UpdatePlayerSpot (EntityRef PlayerSpotEntityId) (EntityRef PlayerSpotEntityId)
    | UpdateDealerRound (EntityRef DealerRoundEntityId) (EntityRef DealerRoundEntityId)
    | UpdateDealer (EntityRef DealerEntityId) (EntityRef DealerEntityId)
    deriving (Eq, Show, Generic)

instance ToJSON DealerHandEntityRelsDelta
instance FromJSON DealerHandEntityRelsDelta

instance Incremental DealerHandEntityRelsDelta where
    type Target DealerHandEntityRelsDelta = DealerHandEntityRels

    applyDelta delta rels = case delta of
        UpdatePlayerSpot _ new ->
            rels{_dealerHandEntityRelsBelongsToPlayerSpot = new}
        UpdateDealerRound _ new ->
            rels{_dealerHandEntityRelsBelongsToDealerRound = new}
        UpdateDealer _ new ->
            rels{_dealerHandEntityRelsOwnedByDealer = new}

    previewDelta delta rels = case delta of
        UpdatePlayerSpot old _ ->
            if _dealerHandEntityRelsBelongsToPlayerSpot rels == old
                then Just $ applyDelta delta rels
                else Nothing
        UpdateDealerRound old _ ->
            if _dealerHandEntityRelsBelongsToDealerRound rels == old
                then Just $ applyDelta delta rels
                else Nothing
        UpdateDealer old _ ->
            if _dealerHandEntityRelsOwnedByDealer rels == old
                then Just $ applyDelta delta rels
                else Nothing

    describeDelta delta _ = case delta of
        UpdatePlayerSpot old new ->
            "Updated PlayerSpot ref: " ++ show old ++ " -> " ++ show new
        UpdateDealerRound old new ->
            "Updated DealerRound ref: " ++ show old ++ " -> " ++ show new
        UpdateDealer old new ->
            "Updated Dealer ref: " ++ show old ++ " -> " ++ show new

instance Reversible DealerHandEntityRelsDelta where
    invert = \case
        UpdatePlayerSpot old new -> Right (UpdatePlayerSpot new old)
        UpdateDealerRound old new -> Right (UpdateDealerRound new old)
        UpdateDealer old new -> Right (UpdateDealer new old)

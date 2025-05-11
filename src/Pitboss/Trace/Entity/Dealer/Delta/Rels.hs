{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.Dealer.Delta.Rels where

import Data.Aeson
import GHC.Generics
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.Dealer
import Pitboss.Trace.Types.EntityRef
import Pitboss.Trace.Types.Identifier

data DealerEntityRelsDelta
    = UpdateRound (Maybe (EntityRef DealerRoundEntityId)) (Maybe (EntityRef DealerRoundEntityId))
    | UpdateHand (Maybe (EntityRef DealerHandEntityId)) (Maybe (EntityRef DealerHandEntityId))
    deriving (Eq, Show, Generic)

instance ToJSON DealerEntityRelsDelta

instance FromJSON DealerEntityRelsDelta

instance Incremental DealerEntityRelsDelta where
    type Target DealerEntityRelsDelta = DealerEntityRels

    applyDelta delta rels = case delta of
        UpdateRound _ new -> rels{_dealerEntityRelsCurrentRound = new}
        UpdateHand _ new -> rels{_dealerEntityRelsActiveHand = new}

    previewDelta delta state = case delta of
        UpdateRound old _ ->
            if _dealerEntityRelsCurrentRound state == old
                then Just $ applyDelta delta state
                else Nothing
        UpdateHand old _ ->
            if _dealerEntityRelsActiveHand state == old
                then Just $ applyDelta delta state
                else Nothing

    describeDelta delta _ = case delta of
        UpdateRound old new ->
            "Updated dealer round: " ++ show old ++ " → " ++ show new
        UpdateHand old new ->
            "Updated dealer hand: " ++ show old ++ " → " ++ show new

instance Reversible DealerEntityRelsDelta where
    invert = \case
        UpdateRound old new -> Right (UpdateRound new old)
        UpdateHand old new -> Right (UpdateHand new old)

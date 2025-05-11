{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.DealerHand.Delta.Rels where

import Data.Aeson
import GHC.Generics
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.DealerHand
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
  type Entity DealerHandEntityRelsDelta = DealerHandEntityRels

  applyDelta delta rels = case delta of
    UpdatePlayerSpot _ new -> rels {_dealerHandEntityRelsBelongsToPlayerSpot = new}
    UpdateDealerRound _ new -> rels {_dealerHandEntityRelsBelongsToDealerRound = new}
    UpdateDealer _ new -> rels {_dealerHandEntityRelsOwnedByDealer = new}

  previewDelta delta rels = case delta of
    UpdatePlayerSpot old _ ->
      if old == _dealerHandEntityRelsBelongsToPlayerSpot rels
        then Just (applyDelta delta rels)
        else Nothing
    UpdateDealerRound old _ ->
      if old == _dealerHandEntityRelsBelongsToDealerRound rels
        then Just (applyDelta delta rels)
        else Nothing
    UpdateDealer old _ ->
      if old == _dealerHandEntityRelsOwnedByDealer rels
        then Just (applyDelta delta rels)
        else Nothing

  describeDelta delta _ = case delta of
    UpdatePlayerSpot old new -> "Changed spot: " ++ show old ++ " → " ++ show new
    UpdateDealerRound old new -> "Changed round: " ++ show old ++ " → " ++ show new
    UpdateDealer old new -> "Changed dealer: " ++ show old ++ " → " ++ show new

instance Reversible DealerHandEntityRelsDelta where
  invert = \case
    UpdatePlayerSpot old new -> Right (UpdatePlayerSpot new old)
    UpdateDealerRound old new -> Right (UpdateDealerRound new old)
    UpdateDealer old new -> Right (UpdateDealer new old)

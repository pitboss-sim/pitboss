{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.DealerHand.Delta.Rels where

import Data.Aeson
import GHC.Generics
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.DealerHand
import Pitboss.Trace.Types.EntityRef
import Pitboss.Trace.Types.Identifier

data DealerHandRelationsDelta
  = UpdatePlayerSpot (EntityRef PlayerSpotId) (EntityRef PlayerSpotId)
  | UpdateRound (EntityRef DealerRoundId) (EntityRef DealerRoundId)
  | UpdateDealer (EntityRef DealerId) (EntityRef DealerId)
  deriving (Eq, Show, Generic)

instance ToJSON DealerHandRelationsDelta

instance FromJSON DealerHandRelationsDelta

instance Incremental DealerHandRelationsDelta where
  type Entity DealerHandRelationsDelta = DealerHandRelations

  applyDelta delta rels = case delta of
    UpdatePlayerSpot _ new -> rels {_dealerHandRelsBelongsToPlayerSpot = new}
    UpdateRound _ new -> rels {_dealerHandRelsBelongsToRound = new}
    UpdateDealer _ new -> rels {_dealerHandRelsOwnedByDealer = new}

  previewDelta delta rels = case delta of
    UpdatePlayerSpot old _ ->
      if old == _dealerHandRelsBelongsToPlayerSpot rels
        then Just (applyDelta delta rels)
        else Nothing
    UpdateRound old _ ->
      if old == _dealerHandRelsBelongsToRound rels
        then Just (applyDelta delta rels)
        else Nothing
    UpdateDealer old _ ->
      if old == _dealerHandRelsOwnedByDealer rels
        then Just (applyDelta delta rels)
        else Nothing

  describeDelta delta _ = case delta of
    UpdatePlayerSpot old new -> "Changed spot: " ++ show old ++ " → " ++ show new
    UpdateRound old new -> "Changed round: " ++ show old ++ " → " ++ show new
    UpdateDealer old new -> "Changed dealer: " ++ show old ++ " → " ++ show new

instance Reversible DealerHandRelationsDelta where
  invert = \case
    UpdatePlayerSpot old new -> Right (UpdatePlayerSpot new old)
    UpdateRound old new -> Right (UpdateRound new old)
    UpdateDealer old new -> Right (UpdateDealer new old)

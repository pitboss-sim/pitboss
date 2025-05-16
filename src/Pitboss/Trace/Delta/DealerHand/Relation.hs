{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Delta.DealerHand.Relation where

import Data.Aeson
import GHC.Generics
import Pitboss.Trace.Delta.Capabilities.Incremental
import Pitboss.Trace.Delta.Capabilities.Reversible
import Pitboss.Trace.Entity.DealerHand
import Pitboss.Trace.Types.Identifier

data DealerHandRelationsDelta
  = UpdatePlayerSpot PlayerSpotId PlayerSpotId
  | UpdateRound DealerRoundId DealerRoundId
  | UpdateDealer DealerId DealerId
  deriving (Eq, Show, Generic)

instance ToJSON DealerHandRelationsDelta

instance FromJSON DealerHandRelationsDelta

instance Incremental DealerHandRelationsDelta where
  type Entity DealerHandRelationsDelta = DealerHandRelations

  applyDelta delta rels = case delta of
    UpdatePlayerSpot _ new -> rels {_belongsToPlayerSpot = new}
    UpdateRound _ new -> rels {_belongsToRound = new}
    UpdateDealer _ new -> rels {_ownedByDealer = new}

  previewDelta delta rels = case delta of
    UpdatePlayerSpot old _ ->
      if old == _belongsToPlayerSpot rels
        then Just (applyDelta delta rels)
        else Nothing
    UpdateRound old _ ->
      if old == _belongsToRound rels
        then Just (applyDelta delta rels)
        else Nothing
    UpdateDealer old _ ->
      if old == _ownedByDealer rels
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

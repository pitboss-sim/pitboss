{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.PlayerHand.Delta.Rels where

import Data.Aeson
import GHC.Generics
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.PlayerHand
import Pitboss.Trace.Types.EntityRef
import Pitboss.Trace.Types.Identifier

data PlayerHandRelationsDelta
  = UpdatePlayerSpot (EntityRef PlayerSpotId) (EntityRef PlayerSpotId)
  | UpdateRound (EntityRef DealerRoundId) (EntityRef DealerRoundId)
  | UpdatePlayer (EntityRef PlayerId) (EntityRef PlayerId)
  deriving (Eq, Show, Generic)

instance Incremental PlayerHandRelationsDelta where
  type Entity PlayerHandRelationsDelta = PlayerHandRelations

  applyDelta delta rels = case delta of
    UpdatePlayerSpot _ new -> rels {_playerHandRelsBelongsToPlayerSpot = new}
    UpdateRound _ new -> rels {_playerHandRelsBelongsToRound = new}
    UpdatePlayer _ new -> rels {_playerHandRelsOwnedByPlayer = new}

  previewDelta delta state = case delta of
    UpdatePlayerSpot old _ ->
      if old == _playerHandRelsBelongsToPlayerSpot state
        then Just $ applyDelta delta state
        else Nothing
    UpdateRound old _ ->
      if old == _playerHandRelsBelongsToRound state
        then Just $ applyDelta delta state
        else Nothing
    UpdatePlayer old _ ->
      if old == _playerHandRelsOwnedByPlayer state
        then Just $ applyDelta delta state
        else Nothing

  describeDelta delta _ = case delta of
    UpdateRound old new -> "Changed round: " ++ show old ++ " → " ++ show new
    UpdatePlayer old new -> "Changed player: " ++ show old ++ " → " ++ show new
    UpdatePlayerSpot old new -> "Changed spot: " ++ show old ++ " → " ++ show new

instance ToJSON PlayerHandRelationsDelta

instance FromJSON PlayerHandRelationsDelta

instance Reversible PlayerHandRelationsDelta where
  invert = \case
    UpdatePlayerSpot old new -> Right (UpdatePlayerSpot new old)
    UpdateRound old new -> Right (UpdateRound new old)
    UpdatePlayer old new -> Right (UpdatePlayer new old)

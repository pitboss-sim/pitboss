{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Delta.PlayerSpot.Relation where

import Data.Aeson
import GHC.Generics
import Pitboss.Trace.Delta.Capabilities.Incremental
import Pitboss.Trace.Delta.Capabilities.Reversible
import Pitboss.Trace.Entity.PlayerSpot
import Pitboss.Trace.Types.Identifier

data PlayerSpotRelationsDelta
  = UpdatePlayer PlayerId PlayerId
  | UpdateRound DealerRoundId DealerRoundId
  deriving (Eq, Show, Generic)

instance ToJSON PlayerSpotRelationsDelta

instance FromJSON PlayerSpotRelationsDelta

instance Incremental PlayerSpotRelationsDelta where
  type Entity PlayerSpotRelationsDelta = PlayerSpotRelations

  applyDelta delta rels = case delta of
    UpdatePlayer _ new -> rels {_playerId = new}
    UpdateRound _ new -> rels {_roundId = new}

  previewDelta delta rels = case delta of
    UpdatePlayer old _ ->
      if _playerId rels == old
        then Just $ applyDelta delta rels
        else Nothing
    UpdateRound old _ ->
      if _roundId rels == old
        then Just $ applyDelta delta rels
        else Nothing

  describeDelta delta _ = case delta of
    UpdatePlayer old new -> "Updated player ID: " ++ show old ++ " → " ++ show new
    UpdateRound old new -> "Updated round ID: " ++ show old ++ " → " ++ show new

instance Reversible PlayerSpotRelationsDelta where
  invert = \case
    UpdatePlayer old new -> Right (UpdatePlayer new old)
    UpdateRound old new -> Right (UpdateRound new old)

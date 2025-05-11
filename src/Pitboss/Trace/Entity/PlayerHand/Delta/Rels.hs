{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.PlayerHand.Delta.Rels where

import Data.Aeson
import GHC.Generics
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.PlayerHand
import Pitboss.Trace.Types.EntityRef
import Pitboss.Trace.Types.Identifier

data PlayerHandEntityRelsDelta
  = UpdatePlayerSpot (EntityRef PlayerSpotEntityId) (EntityRef PlayerSpotEntityId)
  | UpdateDealerRound (EntityRef DealerRoundEntityId) (EntityRef DealerRoundEntityId)
  | UpdatePlayer (EntityRef PlayerEntityId) (EntityRef PlayerEntityId)
  deriving (Eq, Show, Generic)

instance Incremental PlayerHandEntityRelsDelta where
  type Target PlayerHandEntityRelsDelta = PlayerHandEntityRels

  applyDelta delta rels = case delta of
    UpdatePlayerSpot _ new -> rels {_playerHandEntityRelsBelongsToPlayerSpot = new}
    UpdateDealerRound _ new -> rels {_playerHandEntityRelsBelongsToDealerRound = new}
    UpdatePlayer _ new -> rels {_playerHandEntityRelsOwnedByPlayer = new}

  previewDelta delta state = case delta of
    UpdatePlayerSpot old _ ->
      if old == _playerHandEntityRelsBelongsToPlayerSpot state
        then Just $ applyDelta delta state
        else Nothing
    UpdateDealerRound old _ ->
      if old == _playerHandEntityRelsBelongsToDealerRound state
        then Just $ applyDelta delta state
        else Nothing
    UpdatePlayer old _ ->
      if old == _playerHandEntityRelsOwnedByPlayer state
        then Just $ applyDelta delta state
        else Nothing

  describeDelta delta _ = case delta of
    UpdateDealerRound old new -> "Changed round: " ++ show old ++ " → " ++ show new
    UpdatePlayer old new -> "Changed player: " ++ show old ++ " → " ++ show new
    UpdatePlayerSpot old new -> "Changed spot: " ++ show old ++ " → " ++ show new

instance ToJSON PlayerHandEntityRelsDelta

instance FromJSON PlayerHandEntityRelsDelta

instance Reversible PlayerHandEntityRelsDelta where
  invert = \case
    UpdatePlayerSpot old new -> Right (UpdatePlayerSpot new old)
    UpdateDealerRound old new -> Right (UpdateDealerRound new old)
    UpdatePlayer old new -> Right (UpdatePlayer new old)

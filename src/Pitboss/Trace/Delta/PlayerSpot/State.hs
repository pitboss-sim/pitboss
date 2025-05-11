{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Delta.PlayerSpot.State where

import Data.Aeson
import GHC.Generics
import Pitboss.Blackjack.Chips
import Pitboss.Trace.Delta.Capabilities.Incremental
import Pitboss.Trace.Delta.Capabilities.Reversible
import Pitboss.Trace.Entity.PlayerSpot
import Pitboss.Trace.Entity.Types.FiniteMap
import Pitboss.Trace.Entity.Types.FiniteMap.Occupancy
import Pitboss.Trace.Types.Identifier

data PlayerSpotStateDelta
  = ReplaceWager Chips Chips
  | UpdateHandOccupancy (PlayerSpotHandIx, Occupancy PlayerHandId) (PlayerSpotHandIx, Occupancy PlayerHandId)
  deriving (Eq, Show, Generic)

instance ToJSON PlayerSpotStateDelta

instance FromJSON PlayerSpotStateDelta

instance Incremental PlayerSpotStateDelta where
  type Entity PlayerSpotStateDelta = PlayerSpotState

  applyDelta delta state = case delta of
    ReplaceWager _ new -> state {_wager = new}
    UpdateHandOccupancy (_, _) (k, v) ->
      state {_handOccupancy = insertFiniteMap k v (_handOccupancy state)}

  previewDelta delta state = case delta of
    ReplaceWager old _ ->
      if _wager state == old
        then Just $ applyDelta delta state
        else Nothing
    UpdateHandOccupancy (oldK, oldV) _ ->
      case lookupFiniteMap oldK (_handOccupancy state) of
        Just actualV
          | actualV == oldV ->
              Just $ applyDelta delta state
        _ -> Nothing

  describeDelta delta _ = case delta of
    ReplaceWager old new ->
      "Replaced wager: " ++ show old ++ " → " ++ show new
    UpdateHandOccupancy (k1, v1) (k2, v2) ->
      "Updated hand occupancy: " ++ show k1 ++ "=" ++ show v1 ++ " → " ++ show k2 ++ "=" ++ show v2

instance Reversible PlayerSpotStateDelta where
  invert = \case
    ReplaceWager old new -> Right (ReplaceWager new old)
    UpdateHandOccupancy old new -> Right (UpdateHandOccupancy new old)

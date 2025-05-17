{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.PlayerSpot.Delta.Attrs where

import Data.Aeson
import GHC.Generics
import Pitboss.Blackjack.Chips
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.PlayerSpot

data PlayerSpotStateDelta
  = ReplaceWager Chips Chips
  deriving (Eq, Show, Generic)

instance ToJSON PlayerSpotStateDelta

instance FromJSON PlayerSpotStateDelta

instance Incremental PlayerSpotStateDelta where
  type Entity PlayerSpotStateDelta = PlayerSpotState

  applyDelta delta state = case delta of
    ReplaceWager _ new -> state {_playerSpotStateWager = new}

  previewDelta delta state = case delta of
    ReplaceWager old _ ->
      if _playerSpotStateWager state == old
        then Just $ applyDelta delta state
        else Nothing

  describeDelta delta _ = case delta of
    ReplaceWager old new ->
      "Replaced wager: " ++ show old ++ " → " ++ show new

instance Reversible PlayerSpotStateDelta where
  invert = \case
    ReplaceWager old new -> Right (ReplaceWager new old)

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.PlayerSpot.Delta.Attrs where

import Data.Aeson
import GHC.Generics
import Pitboss.Blackjack.Chips
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.PlayerSpot

data PlayerSpotEntityAttrsDelta
  = ReplaceWager Chips Chips
  deriving (Eq, Show, Generic)

instance ToJSON PlayerSpotEntityAttrsDelta

instance FromJSON PlayerSpotEntityAttrsDelta

instance Incremental PlayerSpotEntityAttrsDelta where
  type Entity PlayerSpotEntityAttrsDelta = PlayerSpotEntityAttrs

  applyDelta delta state = case delta of
    ReplaceWager _ new -> state {_playerSpotEntityAttrsWager = new}

  previewDelta delta state = case delta of
    ReplaceWager old _ ->
      if _playerSpotEntityAttrsWager state == old
        then Just $ applyDelta delta state
        else Nothing

  describeDelta delta _ = case delta of
    ReplaceWager old new ->
      "Replaced wager: " ++ show old ++ " → " ++ show new

instance Reversible PlayerSpotEntityAttrsDelta where
  invert = \case
    ReplaceWager old new -> Right (ReplaceWager new old)

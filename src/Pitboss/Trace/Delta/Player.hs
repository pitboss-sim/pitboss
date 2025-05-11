{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Delta.Player where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Chips (Chips)
import Pitboss.Trace.Delta.Capabilities.Incremental
import Pitboss.Trace.Entity.Player

data PlayerDelta
  = RenamePlayer String
  | SetBankroll Chips
  deriving (Eq, Show, Generic)

instance ToJSON PlayerDelta

instance FromJSON PlayerDelta

instance Incremental PlayerDelta where
  type Entity PlayerDelta = Player

  applyDelta d e = e {_state = applyPlayerDelta d (_state e)}

  previewDelta d e = Just (applyDelta d e)

  describeDelta d _ = case d of
    RenamePlayer name -> "Renamed player to " ++ name
    SetBankroll chips -> "Set bankroll to " ++ show chips

applyPlayerDelta :: PlayerDelta -> PlayerState -> PlayerState
applyPlayerDelta d s = case d of
  RenamePlayer name -> s {_playerName = name}
  SetBankroll c -> s {_bankroll = c}

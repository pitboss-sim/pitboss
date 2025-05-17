{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Pitboss.Trace.Entity.Player.Delta
  ( module Pitboss.Trace.Entity.Player.Delta.Attrs,
    module Pitboss.Trace.Entity.Player.Delta.Modes,
    module Pitboss.Trace.Entity.Player.Delta.Rels,
    PlayerDelta (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Chips (Chips)
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.Player
import Pitboss.Trace.Entity.Player.Delta.Attrs
import Pitboss.Trace.Entity.Player.Delta.Modes
import Pitboss.Trace.Entity.Player.Delta.Rels

data PlayerDelta
  = RenamePlayer String
  | SetBankroll Chips
  deriving (Eq, Show, Generic)

instance ToJSON PlayerDelta

instance FromJSON PlayerDelta

instance Incremental PlayerDelta where
  type Entity PlayerDelta = Player

  applyDelta d e = e {_playerState = applyPlayerDelta d (_playerState e)}

  previewDelta d e = Just (applyDelta d e)

  describeDelta d _ = case d of
    RenamePlayer name -> "Renamed player to " ++ name
    SetBankroll chips -> "Set bankroll to " ++ show chips

applyPlayerDelta :: PlayerDelta -> PlayerState -> PlayerState
applyPlayerDelta d s = case d of
  RenamePlayer name -> s {_playerStatePlayerName = name}
  SetBankroll c -> s {_playerStateBankroll = c}

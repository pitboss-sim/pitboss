{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Pitboss.Trace.Entity.Player.Delta
  ( module Pitboss.Trace.Entity.Player.Delta.Attrs,
    module Pitboss.Trace.Entity.Player.Delta.Modes,
    module Pitboss.Trace.Entity.Player.Delta.Rels,
    PlayerEntityDelta (..),
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

data PlayerEntityDelta
  = RenamePlayer String
  | SetBankroll Chips
  deriving (Eq, Show, Generic)

instance ToJSON PlayerEntityDelta

instance FromJSON PlayerEntityDelta

instance Incremental PlayerEntityDelta where
  type Entity PlayerEntityDelta = Player

  applyDelta d e = e {_playerState = applyPlayerEntityDelta d (_playerState e)}

  previewDelta d e = Just (applyDelta d e)

  describeDelta d _ = case d of
    RenamePlayer name -> "Renamed player to " ++ name
    SetBankroll chips -> "Set bankroll to " ++ show chips

applyPlayerEntityDelta :: PlayerEntityDelta -> PlayerState -> PlayerState
applyPlayerEntityDelta d s = case d of
  RenamePlayer name -> s {_playerStatePlayerName = name}
  SetBankroll c -> s {_playerStateBankroll = c}

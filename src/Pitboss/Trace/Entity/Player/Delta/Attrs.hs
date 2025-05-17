{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.Player.Delta.Attrs where

import Data.Aeson
import GHC.Generics
import Pitboss.Blackjack.Chips
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.Player

data PlayerEntityAttrsDelta
  = RenamePlayer String String
  | SetBankroll Chips Chips
  deriving (Eq, Show, Generic)

instance ToJSON PlayerEntityAttrsDelta

instance FromJSON PlayerEntityAttrsDelta

instance Incremental PlayerEntityAttrsDelta where
  type Entity PlayerEntityAttrsDelta = PlayerState

  applyDelta delta s = case delta of
    RenamePlayer _ new -> s {_playerStatePlayerName = new}
    SetBankroll _ new -> s {_playerStateBankroll = new}

  previewDelta d s = Just $ applyDelta d s

  describeDelta d _ = case d of
    RenamePlayer old new -> "Renamed player: " ++ old ++ " → " ++ new
    SetBankroll old new -> "Set bankroll: " ++ show old ++ " → " ++ show new

instance Reversible PlayerEntityAttrsDelta where
  invert = \case
    RenamePlayer old new -> Right (RenamePlayer new old)
    SetBankroll old new -> Right (SetBankroll new old)

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
    type Target PlayerEntityAttrsDelta = PlayerEntityAttrs

    applyDelta :: PlayerEntityAttrsDelta -> PlayerEntityAttrs -> PlayerEntityAttrs
    applyDelta delta state = case delta of
        RenamePlayer _ new -> state{_playerEntityAttrsPlayerName = new}
        SetBankroll _ new -> state{_playerEntityAttrsBankroll = new}

    previewDelta :: PlayerEntityAttrsDelta -> PlayerEntityAttrs -> Maybe PlayerEntityAttrs
    previewDelta delta state = Just $ applyDelta delta state

    describeDelta :: PlayerEntityAttrsDelta -> PlayerEntityAttrs -> String
    describeDelta delta _ = case delta of
        RenamePlayer old new -> "Renamed player: " ++ old ++ " -> " ++ new
        SetBankroll old new -> "Updated bankroll: " ++ show old ++ " -> " ++ show new

instance Reversible PlayerEntityAttrsDelta where
    invert = \case
        RenamePlayer old new -> Right (RenamePlayer new old)
        SetBankroll old new -> Right (SetBankroll new old)

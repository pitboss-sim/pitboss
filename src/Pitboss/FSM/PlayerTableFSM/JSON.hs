{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Pitboss.FSM.PlayerTableFSM.JSON where

import Data.Aeson
import Pitboss.FSM.PlayerTableFSM.Existential
import Pitboss.FSM.PlayerTableFSM.Types

instance ToJSON SomePlayerTableFSM where
  toJSON (SomePlayerTableFSM fsm) = case fsm of
    IdleFSM -> object ["tag" .= String "Idle"]
    ChoosingTableFSM -> object ["tag" .= String "ChoosingTable"]
    PlacingBetFSM -> object ["tag" .= String "PlacingBet"]
    PlayingHandFSM -> object ["tag" .= String "PlayingHand"]
    ObservingFSM -> object ["tag" .= String "Observing"]
    DoneFSM -> object ["tag" .= String "Done"]

instance FromJSON SomePlayerTableFSM where
  parseJSON = withObject "SomePlayerTableFSM" $ \obj -> do
    tag <- obj .: "tag"
    case tag of
      "Idle" -> pure $ SomePlayerTableFSM IdleFSM
      "ChoosingTable" -> pure $ SomePlayerTableFSM ChoosingTableFSM
      "PlacingBet" -> pure $ SomePlayerTableFSM PlacingBetFSM
      "PlayingHand" -> pure $ SomePlayerTableFSM PlayingHandFSM
      "Observing" -> pure $ SomePlayerTableFSM ObservingFSM
      "Done" -> pure $ SomePlayerTableFSM DoneFSM
      _ -> fail $ "Unknown tag for SomePlayerTableFSM: " ++ tag

instance ToJSON (PlayerTableFSM p) where
  toJSON = \case
    IdleFSM -> String "Idle"
    ChoosingTableFSM -> String "ChoosingTable"
    PlacingBetFSM -> String "PlacingBet"
    PlayingHandFSM -> String "PlayingHand"
    ObservingFSM -> String "Observing"
    DoneFSM -> String "Done"

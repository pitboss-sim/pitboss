{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Pitboss.FSM.PlayerTable
  ( module Pitboss.FSM.PlayerTable.FSM,
    module Pitboss.FSM.PlayerTable.Phase,
    module Pitboss.FSM.PlayerTable.Transition,
    SomePlayerTableFSM (..),
    mkPlayerTableFSMIdle,
    mkPlayerTableFSMChoosing,
    mkPlayerTableFSMBetting,
    mkPlayerTableFSMPlaying,
    mkPlayerTableFSMObserving,
    mkPlayerTableFSMDone,
  )
where

import Data.Aeson.Types
import Pitboss.FSM.PlayerTable.FSM
import Pitboss.FSM.PlayerTable.Phase
import Pitboss.FSM.PlayerTable.Transition

mkPlayerTableFSMIdle :: SomePlayerTableFSM
mkPlayerTableFSMIdle = SomePlayerTableFSM IdleFSM

mkPlayerTableFSMChoosing :: SomePlayerTableFSM
mkPlayerTableFSMChoosing = SomePlayerTableFSM ChoosingTableFSM

mkPlayerTableFSMBetting :: SomePlayerTableFSM
mkPlayerTableFSMBetting = SomePlayerTableFSM PlacingBetFSM

mkPlayerTableFSMPlaying :: SomePlayerTableFSM
mkPlayerTableFSMPlaying = SomePlayerTableFSM PlayingHandFSM

mkPlayerTableFSMObserving :: SomePlayerTableFSM
mkPlayerTableFSMObserving = SomePlayerTableFSM ObservingFSM

mkPlayerTableFSMDone :: SomePlayerTableFSM
mkPlayerTableFSMDone = SomePlayerTableFSM DoneFSM

data SomePlayerTableFSM = forall p. SomePlayerTableFSM (PlayerTableFSM p)

instance Show SomePlayerTableFSM where
  show (SomePlayerTableFSM fsm) = show fsm

instance Eq SomePlayerTableFSM where
  SomePlayerTableFSM f1 == SomePlayerTableFSM f2 = case (f1, f2) of
    (IdleFSM, IdleFSM) -> True
    (ChoosingTableFSM, ChoosingTableFSM) -> True
    (PlacingBetFSM, PlacingBetFSM) -> True
    (PlayingHandFSM, PlayingHandFSM) -> True
    (ObservingFSM, ObservingFSM) -> True
    (DoneFSM, DoneFSM) -> True
    _ -> False

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

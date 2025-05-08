{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Pitboss.FSM.PlayerTableFSM.FSM where

import Pitboss.FSM.PlayerTableFSM.Phase
import Pitboss.FSM.Types.Transitionable

data PlayerTableFSM (p :: PlayerPhase) where
  IdleFSM :: PlayerTableFSM 'Idle
  ChoosingTableFSM :: PlayerTableFSM 'ChoosingTable
  PlacingBetFSM :: PlayerTableFSM 'PlacingBet
  PlayingHandFSM :: PlayerTableFSM 'PlayingHand
  ObservingFSM :: PlayerTableFSM 'Observing
  DoneFSM :: PlayerTableFSM 'Done

deriving instance Show (PlayerTableFSM p)

deriving instance Eq (PlayerTableFSM p)

instance Transitionable (PlayerTableFSM p) where
  transitionType = \case
    IdleFSM -> AwaitInput
    ChoosingTableFSM -> AwaitInput
    PlacingBetFSM -> AwaitInput
    PlayingHandFSM -> AwaitInput
    ObservingFSM -> AutoAdvance
    DoneFSM -> TerminalPhase

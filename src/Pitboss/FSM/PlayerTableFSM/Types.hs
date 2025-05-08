{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

module Pitboss.FSM.PlayerTableFSM.Types where

import Pitboss.FSM.Types.Transitionable

data PlayerPhase
  = Idle
  | ChoosingTable
  | PlacingBet
  | PlayingHand
  | Observing
  | Done
  deriving (Eq, Show)

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

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Pitboss.FSM.PlayerSpotFSM.FSM where

import Pitboss.FSM.DealerRoundFSM
import Pitboss.FSM.PlayerSpotFSM.Phase
import Pitboss.FSM.Types.Transitionable

data PlayerSpotFSM (p :: PlayerSpotPhase) where
  SpotIdleFSM :: PlayerSpotFSM 'SpotIdle
  SpotEngagedFSM :: PlayerSpotFSM 'SpotEngaged
  SpotWaitingForHandsFSM :: PlayerSpotFSM 'SpotWaitingForHands
  SpotResolvedFSM :: PlayerSpotFSM 'SpotResolved
  SpotInterruptedFSM :: InterruptReason -> PlayerSpotFSM ('SpotInterrupted r)

deriving instance Show (PlayerSpotFSM p)

deriving instance Eq (PlayerSpotFSM p)

instance Transitionable (PlayerSpotFSM p) where
  transitionType = \case
    SpotIdleFSM -> AwaitInput
    SpotEngagedFSM -> AwaitInput
    SpotWaitingForHandsFSM -> AwaitInput
    SpotResolvedFSM -> TerminalPhase
    SpotInterruptedFSM _ -> AwaitInput

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Pitboss.FSM.PlayerSpot.FSM where

import Pitboss.FSM.PlayerSpot.Phase
import Pitboss.FSM.Types
import Pitboss.FSM.Types.Transitionable

data PlayerSpotFSM (p :: PlayerSpotPhase) where
    PSIdleFSM :: PlayerSpotFSM 'PSIdle
    PSEngagedFSM :: PlayerSpotFSM 'PSEngaged
    PSWaitingForHandsFSM :: PlayerSpotFSM 'PSWaitingForHands
    PSResolvedFSM :: PlayerSpotFSM 'PSResolved
    PSInterruptedFSM :: InterruptReason -> PlayerSpotFSM ('PSInterrupted r)

deriving instance Show (PlayerSpotFSM p)

deriving instance Eq (PlayerSpotFSM p)

instance Transitionable (PlayerSpotFSM p) where
    transitionType = \case
        PSIdleFSM -> AwaitInput
        PSEngagedFSM -> AwaitInput
        PSWaitingForHandsFSM -> AwaitInput
        PSResolvedFSM -> TerminalPhase
        PSInterruptedFSM _ -> AwaitInput

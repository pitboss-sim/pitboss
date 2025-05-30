{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Pitboss.FSM.PlayerTable.FSM where

import Pitboss.FSM.PlayerTable.Phase
import Pitboss.FSM.Types.Transitionable

data PlayerTableFSM (p :: PlayerTablePhase) where
    PTIdleFSM :: PlayerTableFSM 'PTIdle
    PTChoosingTableFSM :: PlayerTableFSM 'PTChoosingTable
    PTPlacingBetFSM :: PlayerTableFSM 'PTPlacingBet
    PTPlayingHandFSM :: PlayerTableFSM 'PTPlayingHand
    PTObservingFSM :: PlayerTableFSM 'PTObserving
    PTDoneFSM :: PlayerTableFSM 'PTDone

deriving instance Show (PlayerTableFSM p)
deriving instance Eq (PlayerTableFSM p)

instance Transitionable (PlayerTableFSM p) where
    transitionType = \case
        PTIdleFSM -> AwaitInput
        PTChoosingTableFSM -> AwaitInput
        PTPlacingBetFSM -> AwaitInput
        PTPlayingHandFSM -> AwaitInput
        PTObservingFSM -> AutoAdvance
        PTDoneFSM -> TerminalPhase

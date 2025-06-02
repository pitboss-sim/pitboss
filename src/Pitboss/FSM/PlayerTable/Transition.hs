{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Pitboss.FSM.PlayerTable.Transition where

import Pitboss.FSM.PlayerTable.FSM
import Pitboss.FSM.PlayerTable.Phase

type family ValidPlayerTableTransition (from :: PlayerTablePhase) (to :: PlayerTablePhase) :: Bool where
    ValidPlayerTableTransition 'PTIdle 'PTChoosingTable = 'True
    ValidPlayerTableTransition 'PTChoosingTable 'PTPlacingBet = 'True
    ValidPlayerTableTransition 'PTPlacingBet 'PTPlayingHand = 'True
    ValidPlayerTableTransition 'PTPlayingHand 'PTObserving = 'True
    ValidPlayerTableTransition 'PTObserving 'PTDone = 'True
    ValidPlayerTableTransition _ _ = 'False

beginChoosingTable ::
    (ValidPlayerTableTransition 'PTIdle 'PTChoosingTable ~ 'True) =>
    PlayerTableFSM 'PTIdle ->
    PlayerTableFSM 'PTChoosingTable
beginChoosingTable PTIdleFSM = PTChoosingTableFSM

confirmTableChoice ::
    (ValidPlayerTableTransition 'PTChoosingTable 'PTPlacingBet ~ 'True) =>
    PlayerTableFSM 'PTChoosingTable ->
    PlayerTableFSM 'PTPlacingBet
confirmTableChoice PTChoosingTableFSM = PTPlacingBetFSM

placeBet ::
    (ValidPlayerTableTransition 'PTPlacingBet 'PTPlayingHand ~ 'True) =>
    PlayerTableFSM 'PTPlacingBet ->
    PlayerTableFSM 'PTPlayingHand
placeBet PTPlacingBetFSM = PTPlayingHandFSM

startObservation ::
    (ValidPlayerTableTransition 'PTPlayingHand 'PTObserving ~ 'True) =>
    PlayerTableFSM 'PTPlayingHand ->
    PlayerTableFSM 'PTObserving
startObservation PTPlayingHandFSM = PTObservingFSM

completeSession ::
    (ValidPlayerTableTransition 'PTObserving 'PTDone ~ 'True) =>
    PlayerTableFSM 'PTObserving ->
    PlayerTableFSM 'PTDone
completeSession PTObservingFSM = PTDoneFSM

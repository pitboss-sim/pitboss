{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Pitboss.FSM.PlayerTable.Transition where

import Pitboss.FSM.PlayerTable.FSM
import Pitboss.FSM.PlayerTable.Phase

type family ValidPlayerTableTransition (from :: PlayerPhase) (to :: PlayerPhase) :: Bool where
    ValidPlayerTableTransition 'Idle 'ChoosingTable = 'True
    ValidPlayerTableTransition 'ChoosingTable 'PlacingBet = 'True
    ValidPlayerTableTransition 'PlacingBet 'PlayingHand = 'True
    ValidPlayerTableTransition 'PlayingHand 'Observing = 'True
    ValidPlayerTableTransition 'Observing 'Done = 'True
    ValidPlayerTableTransition _ _ = 'False

beginChoosingTable ::
    (ValidPlayerTableTransition 'Idle 'ChoosingTable ~ 'True) =>
    PlayerTableFSM 'Idle ->
    PlayerTableFSM 'ChoosingTable
beginChoosingTable IdleFSM = ChoosingTableFSM

confirmTableChoice ::
    (ValidPlayerTableTransition 'ChoosingTable 'PlacingBet ~ 'True) =>
    PlayerTableFSM 'ChoosingTable ->
    PlayerTableFSM 'PlacingBet
confirmTableChoice ChoosingTableFSM = PlacingBetFSM

placeBet ::
    (ValidPlayerTableTransition 'PlacingBet 'PlayingHand ~ 'True) =>
    PlayerTableFSM 'PlacingBet ->
    PlayerTableFSM 'PlayingHand
placeBet PlacingBetFSM = PlayingHandFSM

startObservation ::
    (ValidPlayerTableTransition 'PlayingHand 'Observing ~ 'True) =>
    PlayerTableFSM 'PlayingHand ->
    PlayerTableFSM 'Observing
startObservation PlayingHandFSM = ObservingFSM

completeSession ::
    (ValidPlayerTableTransition 'Observing 'Done ~ 'True) =>
    PlayerTableFSM 'Observing ->
    PlayerTableFSM 'Done
completeSession ObservingFSM = DoneFSM

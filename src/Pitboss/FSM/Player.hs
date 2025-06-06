{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.FSM.Player where

import Pitboss.FSM.Types.Player

type family ValidPlayePransition (from :: PlayerPhase) (to :: PlayerPhase) :: Bool where
    ValidPlayePransition 'PIdle 'PChoosingTable = True
    ValidPlayePransition 'PChoosingTable 'PPlacingBet = True
    ValidPlayePransition 'PPlacingBet 'PPlayingHand = True
    ValidPlayePransition 'PPlayingHand 'PObserving = True
    ValidPlayePransition 'PObserving 'PDone = True
    ValidPlayePransition _ _ = 'False

beginChoosingTable ::
    (ValidPlayePransition 'PIdle 'PChoosingTable ~ True) =>
    PlayerFSM 'PIdle ->
    PlayerFSM 'PChoosingTable
beginChoosingTable PIdleFSM = PChoosingTableFSM

confirPableChoice ::
    (ValidPlayePransition 'PChoosingTable 'PPlacingBet ~ True) =>
    PlayerFSM 'PChoosingTable ->
    PlayerFSM 'PPlacingBet
confirPableChoice PChoosingTableFSM = PPlacingBetFSM

placeBet ::
    (ValidPlayePransition 'PPlacingBet 'PPlayingHand ~ True) =>
    PlayerFSM 'PPlacingBet ->
    PlayerFSM 'PPlayingHand
placeBet PPlacingBetFSM = PPlayingHandFSM

startObservation ::
    (ValidPlayePransition 'PPlayingHand 'PObserving ~ True) =>
    PlayerFSM 'PPlayingHand ->
    PlayerFSM 'PObserving
startObservation PPlayingHandFSM = PObservingFSM

completeSession ::
    (ValidPlayePransition 'PObserving 'PDone ~ True) =>
    PlayerFSM 'PObserving ->
    PlayerFSM 'PDone
completeSession PObservingFSM = PDoneFSM

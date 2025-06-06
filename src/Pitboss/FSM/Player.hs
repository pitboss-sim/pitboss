{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.FSM.Player where

import Pitboss.FSM.Types

type family ValidPlayePransition (from :: PlayerPhase) (to :: PlayerPhase) :: Bool where
    ValidPlayePransition 'PIdle 'PChoosingTable = True
    ValidPlayePransition 'PChoosingTable 'PPlacingBet = True
    ValidPlayePransition 'PPlacingBet 'PPlayingHand = True
    ValidPlayePransition 'PPlayingHand 'PObserving = True
    ValidPlayePransition 'PObserving 'PDone = True
    ValidPlayePransition _ _ = 'False

beginChoosingTable ::
    PlayerFSM 'PIdle ->
    PlayerFSM 'PChoosingTable
beginChoosingTable PIdleFSM = PChoosingTableFSM

confirPableChoice ::
    PlayerFSM 'PChoosingTable ->
    PlayerFSM 'PPlacingBet
confirPableChoice PChoosingTableFSM = PPlacingBetFSM

placeBet ::
    PlayerFSM 'PPlacingBet ->
    PlayerFSM 'PPlayingHand
placeBet PPlacingBetFSM = PPlayingHandFSM

startObservation ::
    PlayerFSM 'PPlayingHand ->
    PlayerFSM 'PObserving
startObservation PPlayingHandFSM = PObservingFSM

completeSession ::
    PlayerFSM 'PObserving ->
    PlayerFSM 'PDone
completeSession PObservingFSM = PDoneFSM

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.FSM.Contestant.Bout where

import Pitboss.FSM.Types.Contestant

type family ValidContestantBoutTransition (from :: ContestantBoutPhase) (to :: ContestantBoutPhase) :: Bool where
    ValidContestantBoutTransition 'CBAwaitingCards 'CBPlaying = 'True
    ValidContestantBoutTransition 'CBPlaying 'CBAwaitingDealer = 'True
    ValidContestantBoutTransition 'CBAwaitingDealer 'CBResolved = 'True
    ValidContestantBoutTransition 'CBResolved 'CBSettled = 'True
    ValidContestantBoutTransition _ _ = 'False

beginPlaying ::
    (ValidContestantBoutTransition 'CBAwaitingCards 'CBPlaying ~ 'True) =>
    ContestantBoutFSM 'CBAwaitingCards ->
    ContestantBoutFSM 'CBPlaying
beginPlaying CBAwaitingCardsFSM = CBPlayingFSM

handComplete ::
    (ValidContestantBoutTransition 'CBPlaying 'CBAwaitingDealer ~ 'True) =>
    ContestantBoutFSM 'CBPlaying ->
    ContestantBoutFSM 'CBAwaitingDealer
handComplete CBPlayingFSM = CBAwaitingDealerFSM

contestantDealerComplete ::
    (ValidContestantBoutTransition 'CBAwaitingDealer 'CBResolved ~ 'True) =>
    ContestantBoutFSM 'CBAwaitingDealer ->
    ContestantBoutFSM 'CBResolved
contestantDealerComplete CBAwaitingDealerFSM = CBResolvedFSM

settleBout ::
    (ValidContestantBoutTransition 'CBResolved 'CBSettled ~ 'True) =>
    ContestantBoutFSM 'CBResolved ->
    ContestantBoutFSM 'CBSettled
settleBout CBResolvedFSM = CBSettledFSM

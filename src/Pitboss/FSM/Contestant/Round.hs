{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.FSM.Contestant.Round where

import Pitboss.Blackjack
import Pitboss.FSM.Types.Contestant

type family ValidContestantRoundTransition (from :: ContestantRoundPhase) (to :: ContestantRoundPhase) :: Bool where
    ValidContestantRoundTransition 'CRIdle 'CREngaged = 'True
    ValidContestantRoundTransition 'CREngaged 'CRWaitingForHands = 'True
    ValidContestantRoundTransition 'CRWaitingForHands 'CRResolved = 'True
    ValidContestantRoundTransition p ('CRInterrupted r) = 'True
    ValidContestantRoundTransition ('CRInterrupted r) 'CRIdle = 'True
    ValidContestantRoundTransition _ _ = 'False

beginEngagement ::
    (ValidContestantRoundTransition 'CRIdle 'CREngaged ~ 'True) =>
    ContestantRoundFSM 'CRIdle ->
    ContestantRoundFSM 'CREngaged
beginEngagement CRIdleFSM = CREngagedFSM

beginWaitingForHands ::
    (ValidContestantRoundTransition 'CREngaged 'CRWaitingForHands ~ 'True) =>
    ContestantRoundFSM 'CREngaged ->
    ContestantRoundFSM 'CRWaitingForHands
beginWaitingForHands CREngagedFSM = CRWaitingForHandsFSM

resolveRound ::
    (ValidContestantRoundTransition 'CRWaitingForHands 'CRResolved ~ 'True) =>
    ContestantRoundFSM 'CRWaitingForHands ->
    ContestantRoundFSM 'CRResolved
resolveRound CRWaitingForHandsFSM = CRResolvedFSM

interruptRound ::
    (ValidContestantRoundTransition from ('CRInterrupted r) ~ 'True) =>
    InterruptReason ->
    ContestantRoundFSM from ->
    ContestantRoundFSM ('CRInterrupted r)
interruptRound reason _ = CRInterruptedFSM reason

contestantRoundResumeFromInterrupt ::
    (ValidContestantRoundTransition ('CRInterrupted r) 'CRIdle ~ 'True) =>
    ContestantRoundFSM ('CRInterrupted r) ->
    ContestantRoundFSM 'CRIdle
contestantRoundResumeFromInterrupt (CRInterruptedFSM _) = CRIdleFSM

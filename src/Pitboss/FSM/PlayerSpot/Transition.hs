{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Pitboss.FSM.PlayerSpot.Transition where

import Pitboss.FSM.PlayerSpot.FSM
import Pitboss.FSM.PlayerSpot.Phase
import Pitboss.FSM.Types

type family ValidPlayerSpotTransition (from :: PlayerSpotPhase) (to :: PlayerSpotPhase) :: Bool where
    ValidPlayerSpotTransition 'SpotIdle 'SpotEngaged = 'True
    ValidPlayerSpotTransition 'SpotEngaged 'SpotWaitingForHands = 'True
    ValidPlayerSpotTransition 'SpotWaitingForHands 'SpotResolved = 'True
    ValidPlayerSpotTransition p ('SpotInterrupted r) = 'True
    ValidPlayerSpotTransition ('SpotInterrupted r) 'SpotIdle = 'True
    ValidPlayerSpotTransition _ _ = 'False

beginEngagement ::
    (ValidPlayerSpotTransition 'SpotIdle 'SpotEngaged ~ 'True) =>
    PlayerSpotFSM 'SpotIdle ->
    PlayerSpotFSM 'SpotEngaged
beginEngagement SpotIdleFSM = SpotEngagedFSM

beginWaitingForHands ::
    (ValidPlayerSpotTransition 'SpotEngaged 'SpotWaitingForHands ~ 'True) =>
    PlayerSpotFSM 'SpotEngaged ->
    PlayerSpotFSM 'SpotWaitingForHands
beginWaitingForHands SpotEngagedFSM = SpotWaitingForHandsFSM

resolveSpot ::
    (ValidPlayerSpotTransition 'SpotWaitingForHands 'SpotResolved ~ 'True) =>
    PlayerSpotFSM 'SpotWaitingForHands ->
    PlayerSpotFSM 'SpotResolved
resolveSpot SpotWaitingForHandsFSM = SpotResolvedFSM

interruptSpot ::
    (ValidPlayerSpotTransition from ('SpotInterrupted r) ~ 'True) =>
    InterruptReason ->
    PlayerSpotFSM from ->
    PlayerSpotFSM ('SpotInterrupted r)
interruptSpot reason _ = SpotInterruptedFSM reason

resumeFromInterrupt ::
    (ValidPlayerSpotTransition ('SpotInterrupted r) 'SpotIdle ~ 'True) =>
    PlayerSpotFSM ('SpotInterrupted r) ->
    PlayerSpotFSM 'SpotIdle
resumeFromInterrupt (SpotInterruptedFSM _) = SpotIdleFSM

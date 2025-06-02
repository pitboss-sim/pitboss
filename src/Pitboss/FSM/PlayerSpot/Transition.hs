{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Pitboss.FSM.PlayerSpot.Transition where

import Pitboss.FSM.PlayerSpot.FSM
import Pitboss.FSM.PlayerSpot.Phase
import Pitboss.FSM.Types

type family ValidPlayerSpotTransition (from :: PlayerSpotPhase) (to :: PlayerSpotPhase) :: Bool where
    ValidPlayerSpotTransition 'PSIdle 'PSEngaged = 'True
    ValidPlayerSpotTransition 'PSEngaged 'PSWaitingForHands = 'True
    ValidPlayerSpotTransition 'PSWaitingForHands 'PSResolved = 'True
    ValidPlayerSpotTransition p ('PSInterrupted r) = 'True
    ValidPlayerSpotTransition ('PSInterrupted r) 'PSIdle = 'True
    ValidPlayerSpotTransition _ _ = 'False

beginEngagement ::
    (ValidPlayerSpotTransition 'PSIdle 'PSEngaged ~ 'True) =>
    PlayerSpotFSM 'PSIdle ->
    PlayerSpotFSM 'PSEngaged
beginEngagement PSIdleFSM = PSEngagedFSM

beginWaitingForHands ::
    (ValidPlayerSpotTransition 'PSEngaged 'PSWaitingForHands ~ 'True) =>
    PlayerSpotFSM 'PSEngaged ->
    PlayerSpotFSM 'PSWaitingForHands
beginWaitingForHands PSEngagedFSM = PSWaitingForHandsFSM

resolveSpot ::
    (ValidPlayerSpotTransition 'PSWaitingForHands 'PSResolved ~ 'True) =>
    PlayerSpotFSM 'PSWaitingForHands ->
    PlayerSpotFSM 'PSResolved
resolveSpot PSWaitingForHandsFSM = PSResolvedFSM

interruptSpot ::
    (ValidPlayerSpotTransition from ('PSInterrupted r) ~ 'True) =>
    InterruptReason ->
    PlayerSpotFSM from ->
    PlayerSpotFSM ('PSInterrupted r)
interruptSpot reason _ = PSInterruptedFSM reason

resumeFromInterrupt ::
    (ValidPlayerSpotTransition ('PSInterrupted r) 'PSIdle ~ 'True) =>
    PlayerSpotFSM ('PSInterrupted r) ->
    PlayerSpotFSM 'PSIdle
resumeFromInterrupt (PSInterruptedFSM _) = PSIdleFSM

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Pitboss.FSM.PlayerSpot.Transition where

import Pitboss.FSM.PlayerSpot.FSM
import Pitboss.FSM.PlayerSpot.Phase

beginEngagement :: PlayerSpotFSM 'SpotIdle -> PlayerSpotFSM 'SpotEngaged
beginEngagement SpotIdleFSM = SpotEngagedFSM

beginWaitingForHands :: PlayerSpotFSM 'SpotEngaged -> PlayerSpotFSM 'SpotWaitingForHands
beginWaitingForHands SpotEngagedFSM = SpotWaitingForHandsFSM

resolveSpot :: PlayerSpotFSM 'SpotWaitingForHands -> PlayerSpotFSM 'SpotResolved
resolveSpot SpotWaitingForHandsFSM = SpotResolvedFSM

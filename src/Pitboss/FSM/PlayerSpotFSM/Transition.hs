{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Pitboss.FSM.PlayerSpotFSM.Transition where

import Pitboss.FSM.PlayerSpotFSM.FSM
import Pitboss.FSM.PlayerSpotFSM.Phase

beginEngagement :: PlayerSpotFSM 'SpotIdle -> PlayerSpotFSM 'SpotEngaged
beginEngagement SpotIdleFSM = SpotEngagedFSM

beginWaitingForHands :: PlayerSpotFSM 'SpotEngaged -> PlayerSpotFSM 'SpotWaitingForHands
beginWaitingForHands SpotEngagedFSM = SpotWaitingForHandsFSM

resolveSpot :: PlayerSpotFSM 'SpotWaitingForHands -> PlayerSpotFSM 'SpotResolved
resolveSpot SpotWaitingForHandsFSM = SpotResolvedFSM

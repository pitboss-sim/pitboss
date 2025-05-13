{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Pitboss.FSM.PlayerSpotFSM
  ( module Pitboss.FSM.PlayerSpotFSM.FSM,
    module Pitboss.FSM.PlayerSpotFSM.Phase,
    module Pitboss.FSM.PlayerSpotFSM.Transition,
    SomePlayerSpotFSM (..),
    mkPlayerSpotFSMIdle,
    mkPlayerSpotFSMEngaged,
    mkPlayerSpotFSMWaiting,
    mkPlayerSpotFSMResolved,
    mkPlayerSpotFSMInterrupted,
    interruptSpot,
    resumeFromInterrupt,
  )
where

import Data.Aeson.Types
import Data.Text qualified as T
import Pitboss.FSM.DealerRoundFSM.Phase
import Pitboss.FSM.PlayerSpotFSM.FSM
import Pitboss.FSM.PlayerSpotFSM.Phase
import Pitboss.FSM.PlayerSpotFSM.Transition
import Pitboss.FSM.Types.Transitionable

mkPlayerSpotFSMIdle :: SomePlayerSpotFSM
mkPlayerSpotFSMIdle = SomePlayerSpotFSM SpotIdleFSM

mkPlayerSpotFSMEngaged :: SomePlayerSpotFSM
mkPlayerSpotFSMEngaged = SomePlayerSpotFSM SpotEngagedFSM

mkPlayerSpotFSMWaiting :: SomePlayerSpotFSM
mkPlayerSpotFSMWaiting = SomePlayerSpotFSM SpotWaitingForHandsFSM

mkPlayerSpotFSMResolved :: SomePlayerSpotFSM
mkPlayerSpotFSMResolved = SomePlayerSpotFSM SpotResolvedFSM

mkPlayerSpotFSMInterrupted :: InterruptReason -> SomePlayerSpotFSM
mkPlayerSpotFSMInterrupted reason = SomePlayerSpotFSM (SpotInterruptedFSM reason)

data SomePlayerSpotFSM = forall p. SomePlayerSpotFSM (PlayerSpotFSM p)

instance Show SomePlayerSpotFSM where
  show (SomePlayerSpotFSM fsm) = show fsm

instance Eq SomePlayerSpotFSM where
  SomePlayerSpotFSM f1 == SomePlayerSpotFSM f2 = case (f1, f2) of
    (SpotIdleFSM, SpotIdleFSM) -> True
    (SpotEngagedFSM, SpotEngagedFSM) -> True
    (SpotWaitingForHandsFSM, SpotWaitingForHandsFSM) -> True
    (SpotResolvedFSM, SpotResolvedFSM) -> True
    (SpotInterruptedFSM r1, SpotInterruptedFSM r2) -> r1 == r2
    _ -> False

instance ToJSON SomePlayerSpotFSM where
  toJSON (SomePlayerSpotFSM fsm) = case fsm of
    SpotIdleFSM -> object ["tag" .= String "SpotIdle"]
    SpotEngagedFSM -> object ["tag" .= String "SpotEngaged"]
    SpotWaitingForHandsFSM -> object ["tag" .= String "SpotWaitingForHands"]
    SpotResolvedFSM -> object ["tag" .= String "SpotResolved"]
    SpotInterruptedFSM reason ->
      object
        [ "tag" .= String "SpotInterrupted",
          "reason" .= reason
        ]

instance FromJSON SomePlayerSpotFSM where
  parseJSON = withObject "SomePlayerSpotFSM" $ \obj -> do
    tag <- obj .: "tag"
    case (tag :: T.Text) of
      "SpotIdle" -> pure $ SomePlayerSpotFSM SpotIdleFSM
      "SpotEngaged" -> pure $ SomePlayerSpotFSM SpotEngagedFSM
      "SpotWaitingForHands" -> pure $ SomePlayerSpotFSM SpotWaitingForHandsFSM
      "SpotResolved" -> pure $ SomePlayerSpotFSM SpotResolvedFSM
      "SpotInterrupted" -> do
        reason <- obj .: "reason"
        pure $ SomePlayerSpotFSM (SpotInterruptedFSM reason)
      other -> fail $ "Unknown tag for SomePlayerSpotFSM: " ++ T.unpack other

instance Transitionable SomePlayerSpotFSM where
  transitionType (SomePlayerSpotFSM fsm) = transitionType fsm

-- helpers

interruptSpot :: InterruptReason -> PlayerSpotFSM p -> SomePlayerSpotFSM
interruptSpot reason _ = SomePlayerSpotFSM (SpotInterruptedFSM reason)

resumeFromInterrupt :: PlayerSpotFSM ('SpotInterrupted r) -> PlayerSpotFSM 'SpotIdle
resumeFromInterrupt (SpotInterruptedFSM _) = SpotIdleFSM

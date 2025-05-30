{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.FSM.PlayerSpot where

import Data.Aeson.Types
import Data.Text qualified as T
import Pitboss.FSM.Types
import GHC.Generics (Generic)

data PlayerSpotPhase
    = PSIdle
    | PSEngaged
    | PSWaitingForHands
    | PSResolved
    | PSInterrupted InterruptReason
    deriving (Eq, Show, Generic)

instance ToJSON PlayerSpotPhase where
    toJSON = \case
        PSIdle -> object ["tag" .= String "Idle"]
        PSEngaged -> object ["tag" .= String "Engaged"]
        PSWaitingForHands -> object ["tag" .= String "WaitingForHands"]
        PSResolved -> object ["tag" .= String "Resolved"]
        PSInterrupted reason -> object ["tag" .= String "Interrupted", "reason" .= reason]

instance FromJSON PlayerSpotPhase where
    parseJSON = withObject "PlayerSpotPhase" $ \obj -> do
        tag <- obj .: "tag"
        case tag :: T.Text of
            "Idle" -> pure PSIdle
            "Engaged" -> pure PSEngaged
            "WaitingForHands" -> pure PSWaitingForHands
            "Resolved" -> pure PSResolved
            "Interrupted" -> PSInterrupted <$> obj .: "reason"
            _ -> fail $ "Unknown tag in PlayerSpotPhase object: " ++ T.unpack tag

data SomePlayerSpotFSM = forall p. SomePlayerSpotFSM (PlayerSpotFSM p)

data PlayerSpotFSM (p :: PlayerSpotPhase) where
    PSIdleFSM :: PlayerSpotFSM 'PSIdle
    PSEngagedFSM :: PlayerSpotFSM 'PSEngaged
    PSWaitingForHandsFSM :: PlayerSpotFSM 'PSWaitingForHands
    PSResolvedFSM :: PlayerSpotFSM 'PSResolved
    PSInterruptedFSM :: InterruptReason -> PlayerSpotFSM ('PSInterrupted r)

deriving instance Show (PlayerSpotFSM p)

deriving instance Eq (PlayerSpotFSM p)

instance Show SomePlayerSpotFSM where
    show (SomePlayerSpotFSM fsm) = show fsm

instance Eq SomePlayerSpotFSM where
    SomePlayerSpotFSM f1 == SomePlayerSpotFSM f2 = case (f1, f2) of
        (PSIdleFSM, PSIdleFSM) -> True
        (PSEngagedFSM, PSEngagedFSM) -> True
        (PSWaitingForHandsFSM, PSWaitingForHandsFSM) -> True
        (PSResolvedFSM, PSResolvedFSM) -> True
        (PSInterruptedFSM r1, PSInterruptedFSM r2) -> r1 == r2
        _ -> False

instance ToJSON SomePlayerSpotFSM where
    toJSON (SomePlayerSpotFSM fsm) = case fsm of
        PSIdleFSM -> object ["tag" .= String "Idle"]
        PSEngagedFSM -> object ["tag" .= String "Engaged"]
        PSWaitingForHandsFSM -> object ["tag" .= String "WaitingForHands"]
        PSResolvedFSM -> object ["tag" .= String "Resolved"]
        PSInterruptedFSM reason ->
            object
                [ "tag" .= String "Interrupted"
                , "reason" .= reason
                ]

instance FromJSON SomePlayerSpotFSM where
    parseJSON = withObject "SomePlayerSpotFSM" $ \obj -> do
        tag <- obj .: "tag"
        case (tag :: T.Text) of
            "Idle" -> pure $ SomePlayerSpotFSM PSIdleFSM
            "Engaged" -> pure $ SomePlayerSpotFSM PSEngagedFSM
            "WaitingForHands" -> pure $ SomePlayerSpotFSM PSWaitingForHandsFSM
            "Resolved" -> pure $ SomePlayerSpotFSM PSResolvedFSM
            "Interrupted" -> do
                reason <- obj .: "reason"
                pure $ SomePlayerSpotFSM (PSInterruptedFSM reason)
            other -> fail $ "Unknown tag for SomePlayerSpotFSM: " ++ T.unpack other

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

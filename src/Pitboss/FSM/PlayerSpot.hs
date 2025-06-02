{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Pitboss.FSM.PlayerSpot (
    module Pitboss.FSM.PlayerSpot.FSM,
    module Pitboss.FSM.PlayerSpot.Phase,
    module Pitboss.FSM.PlayerSpot.Transition,
    SomePlayerSpotFSM (..),
)
where

import Data.Aeson.Types
import Data.Text qualified as T
import Pitboss.FSM.PlayerSpot.FSM
import Pitboss.FSM.PlayerSpot.Phase
import Pitboss.FSM.PlayerSpot.Transition
import Pitboss.FSM.Types.Transitionable

data SomePlayerSpotFSM = forall p. SomePlayerSpotFSM (PlayerSpotFSM p)

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

instance Transitionable SomePlayerSpotFSM where
    transitionType (SomePlayerSpotFSM fsm) = transitionType fsm

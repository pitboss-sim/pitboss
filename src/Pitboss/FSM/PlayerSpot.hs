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
                [ "tag" .= String "SpotInterrupted"
                , "reason" .= reason
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

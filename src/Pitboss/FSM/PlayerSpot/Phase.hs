{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Pitboss.FSM.PlayerSpot.Phase where

import Data.Aeson.Types
import Data.Text qualified as T
import GHC.Generics
import Pitboss.FSM.Types

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

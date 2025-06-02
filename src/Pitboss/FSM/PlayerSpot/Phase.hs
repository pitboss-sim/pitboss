{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Pitboss.FSM.PlayerSpot.Phase where

import Data.Aeson.Types
import Data.Text qualified as T
import GHC.Generics
import Pitboss.FSM.Types

data PlayerSpotPhase
    = SpotIdle
    | SpotEngaged
    | SpotWaitingForHands
    | SpotResolved
    | SpotInterrupted InterruptReason
    deriving (Eq, Show, Generic)

instance ToJSON PlayerSpotPhase where
    toJSON = \case
        SpotIdle -> object ["tag" .= String "SpotIdle"]
        SpotEngaged -> object ["tag" .= String "SpotEngaged"]
        SpotWaitingForHands -> object ["tag" .= String "SpotWaitingForHands"]
        SpotResolved -> object ["tag" .= String "SpotResolved"]
        SpotInterrupted reason -> object ["tag" .= String "SpotInterrupted", "reason" .= reason]

instance FromJSON PlayerSpotPhase where
    parseJSON = withObject "PlayerSpotPhase" $ \obj -> do
        tag <- obj .: "tag"
        case tag :: T.Text of
            "SpotIdle" -> pure SpotIdle
            "SpotEngaged" -> pure SpotEngaged
            "SpotWaitingForHands" -> pure SpotWaitingForHands
            "SpotResolved" -> pure SpotResolved
            "SpotInterrupted" -> SpotInterrupted <$> obj .: "reason"
            _ -> fail $ "Unknown tag in PlayerSpotPhase object: " ++ T.unpack tag

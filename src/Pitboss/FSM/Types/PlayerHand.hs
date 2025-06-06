{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Pitboss.FSM.Types.PlayerHand where

import Data.Aeson.Types
import Pitboss.FSM.Types.Core

data SomePlayerHandFSM = forall p h d s. SomePlayerHandFSM (PlayerHandFSM p h d s)

instance Show SomePlayerHandFSM where
    show (SomePlayerHandFSM fsm) = "SomePlayerHandFSM (" ++ show fsm ++ ")"

instance Eq SomePlayerHandFSM where
    (SomePlayerHandFSM f1) == (SomePlayerHandFSM f2) = case (f1, f2) of
        (PHAwaitingFirstCardFSM, PHAwaitingFirstCardFSM) -> True
        (PHAwaitingSecondCardFSM, PHAwaitingSecondCardFSM) -> True
        (PHDecisionFSM, PHDecisionFSM) -> True
        (PHHittingFSM, PHHittingFSM) -> True
        (PHAwaitingOneCardFSM _, PHAwaitingOneCardFSM _) -> True
        (PHResolvedFSM r1, PHResolvedFSM r2) -> r1 == r2
        (PHAbandonedFSM r1, PHAbandonedFSM r2) -> r1 == r2
        _ -> False

instance ToJSON SomePlayerHandFSM where
    toJSON (SomePlayerHandFSM fsm) = case fsm of
        PHAwaitingFirstCardFSM -> object ["tag" .= String "AwaitingFirstCard"]
        PHAwaitingSecondCardFSM -> object ["tag" .= String "AwaitingSecondCard"]
        PHDecisionFSM -> object ["tag" .= String "Decision"]
        PHHittingFSM -> object ["tag" .= String "Hitting"]
        PHAwaitingOneCardFSM r -> object ["tag" .= String "AwaitingOneCard", "reason" .= r]
        PHResolvedFSM r -> object ["tag" .= String "Resolved", "resolution" .= r]
        PHAbandonedFSM r -> object ["tag" .= String "Abandoned", "reason" .= r]

instance FromJSON SomePlayerHandFSM where
    parseJSON = withObject "SomePlayerHandFSM" $ \obj -> do
        tag <- obj .: "tag"
        case tag of
            "AwaitingFirstCard" -> pure $ SomePlayerHandFSM PHAwaitingFirstCardFSM
            "AwaitingSecondCard" -> pure $ SomePlayerHandFSM PHAwaitingSecondCardFSM
            "Decision" -> pure $ SomePlayerHandFSM PHDecisionFSM
            "Hitting" -> pure $ SomePlayerHandFSM PHHittingFSM
            "AwaitingOneCard" -> do
                r <- obj .: "reason"
                pure $ SomePlayerHandFSM (PHAwaitingOneCardFSM r)
            "Resolved" -> do
                r <- obj .: "resolution"
                pure $ SomePlayerHandFSM (PHResolvedFSM r)
            "Abandoned" -> do
                r <- obj .: "reason"
                pure $ SomePlayerHandFSM (PHAbandonedFSM r)
            other -> fail $ "Unknown tag for SomePlayerHandFSM: " ++ other

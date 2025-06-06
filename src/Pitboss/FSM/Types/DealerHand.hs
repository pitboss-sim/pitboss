{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Pitboss.FSM.Types.DealerHand where

import Data.Aeson.Types
import Pitboss.FSM.Types.Core

data SomeDealerHandFSM = forall p. SomeDealerHandFSM (DealerHandFSM p)

instance Show SomeDealerHandFSM where
    show (SomeDealerHandFSM fsm) = "SomeDealerHandFSM (" ++ show fsm ++ ")"

instance Eq SomeDealerHandFSM where
    (SomeDealerHandFSM f1) == (SomeDealerHandFSM f2) = case (f1, f2) of
        (DHAwaitingFirstCardFSM, DHAwaitingFirstCardFSM) -> True
        (DHAwaitingSecondCardFSM, DHAwaitingSecondCardFSM) -> True
        (DHDealingFSM, DHDealingFSM) -> True
        (DHEvaluatingFSM, DHEvaluatingFSM) -> True
        (DHPlayingFSM, DHPlayingFSM) -> True
        (DHResolvedFSM r1, DHResolvedFSM r2) -> r1 == r2
        (DHInterruptedFSM r1, DHInterruptedFSM r2) -> r1 == r2
        _ -> False

instance ToJSON SomeDealerHandFSM where
    toJSON (SomeDealerHandFSM fsm) = case fsm of
        DHAwaitingFirstCardFSM -> object ["tag" .= String "AwaitingFirstCard"]
        DHAwaitingSecondCardFSM -> object ["tag" .= String "AwaitingSecondCard"]
        DHDealingFSM -> object ["tag" .= String "Dealing"]
        DHEvaluatingFSM -> object ["tag" .= String "Evaluating"]
        DHPlayingFSM -> object ["tag" .= String "Playing"]
        DHResolvedFSM r -> object ["tag" .= String "Resolved", "resolution" .= r]
        DHInterruptedFSM r -> object ["tag" .= String "Interrupted", "reason" .= r]

instance FromJSON SomeDealerHandFSM where
    parseJSON = withObject "SomeDealerHandFSM" $ \obj -> do
        tag <- obj .: "tag"
        case tag of
            "AwaitingFirstCard" -> pure $ SomeDealerHandFSM DHAwaitingFirstCardFSM
            "AwaitingSecondCard" -> pure $ SomeDealerHandFSM DHAwaitingSecondCardFSM
            "Dealing" -> pure $ SomeDealerHandFSM DHDealingFSM
            "Evaluating" -> pure $ SomeDealerHandFSM DHEvaluatingFSM
            "Playing" -> pure $ SomeDealerHandFSM DHPlayingFSM
            "Resolved" -> do
                r <- obj .: "resolution"
                pure $ SomeDealerHandFSM (DHResolvedFSM r)
            "Interrupted" -> do
                r <- obj .: "reason"
                pure $ SomeDealerHandFSM (DHInterruptedFSM r)
            other -> fail $ "Unknown tag for SomeDealerHandFSM: " ++ other

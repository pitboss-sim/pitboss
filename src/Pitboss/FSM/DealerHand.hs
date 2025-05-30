{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Pitboss.FSM.DealerHand (
    module Pitboss.FSM.DealerHand.FSM,
    module Pitboss.FSM.DealerHand.Phase,
    module Pitboss.FSM.DealerHand.Transition,
    SomeDealerHandFSM (..),
)
where

import Data.Aeson.Types
import Pitboss.FSM.DealerHand.FSM
import Pitboss.FSM.DealerHand.Phase
import Pitboss.FSM.DealerHand.Transition

data SomeDealerHandFSM = forall p. SomeDealerHandFSM (DealerHandFSM p)

instance Show SomeDealerHandFSM where
    show (SomeDealerHandFSM fsm) = "SomeDealerHandFSM (" ++ show fsm ++ ")"

instance Eq SomeDealerHandFSM where
    (SomeDealerHandFSM f1) == (SomeDealerHandFSM f2) = case (f1, f2) of
        (DHDealingFSM, DHDealingFSM) -> True
        (DHEvaluatingFSM, DHEvaluatingFSM) -> True
        (DHResolvedFSM r1, DHResolvedFSM r2) -> r1 == r2
        _ -> False

instance ToJSON SomeDealerHandFSM where
    toJSON (SomeDealerHandFSM fsm) = case fsm of
        DHDealingFSM -> object ["tag" .= String "Dealing"]
        DHEvaluatingFSM -> object ["tag" .= String "Evaluating"]
        DHResolvedFSM r -> object ["tag" .= String "Resolved", "resolution" .= r]
        DHInterruptedFSM r -> object ["tag" .= String "Interrupted", "reason" .= r]

instance FromJSON SomeDealerHandFSM where
    parseJSON = withObject "SomeDealerHandFSM" $ \obj -> do
        tag <- obj .: "tag"
        case tag of
            "Dealing" -> pure $ SomeDealerHandFSM DHDealingFSM
            "Evaluating" -> pure $ SomeDealerHandFSM DHEvaluatingFSM
            "Resolved" -> do
                r <- obj .: "resolution"
                pure $ SomeDealerHandFSM (DHResolvedFSM r)
            other -> fail $ "Unknown tag for SomeDealerHandFSM: " ++ other

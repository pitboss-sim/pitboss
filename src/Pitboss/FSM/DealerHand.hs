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
        (DealingFSM, DealingFSM) -> True
        (EvaluatingFSM, EvaluatingFSM) -> True
        (ResolvedFSM r1, ResolvedFSM r2) -> r1 == r2
        _ -> False

instance ToJSON SomeDealerHandFSM where
    toJSON (SomeDealerHandFSM fsm) = case fsm of
        DealingFSM -> object ["tag" .= String "Dealing"]
        EvaluatingFSM -> object ["tag" .= String "Evaluating"]
        ResolvedFSM r -> object ["tag" .= String "Resolved", "resolution" .= r]
        InterruptedFSM r -> object ["tag" .= String "Interrupted", "reason" .= r]

instance FromJSON SomeDealerHandFSM where
    parseJSON = withObject "SomeDealerHandFSM" $ \obj -> do
        tag <- obj .: "tag"
        case tag of
            "Dealing" -> pure $ SomeDealerHandFSM DealingFSM
            "Evaluating" -> pure $ SomeDealerHandFSM EvaluatingFSM
            "Resolved" -> do
                r <- obj .: "resolution"
                pure $ SomeDealerHandFSM (ResolvedFSM r)
            other -> fail $ "Unknown tag for SomeDealerHandFSM: " ++ other

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Pitboss.FSM.DealerHand (
    module Pitboss.FSM.DealerHand.FSM,
    module Pitboss.FSM.DealerHand.Phase,
    module Pitboss.FSM.DealerHand.Transition,
    SomeDealerHandFSM (..),
    mkDealerHandFSMDealing,
    mkDealerHandFSMEvaluating,
    mkDealerHandFSMResolved,
    mkDealerHandFSMInterrupted,
)
where

import Pitboss.Blackjack.Hand

import Data.Aeson.Types
import Pitboss.Blackjack.Offering.RuleSet
import Pitboss.FSM.DealerHand.FSM (DealerHandFSM (..))
import Pitboss.FSM.DealerHand.Phase (DealerHandPhase (..), DealerHandResolution (..))
import Pitboss.FSM.DealerHand.Transition
import Pitboss.FSM.Types (InterruptReason)

mkDealerHandFSMDealing :: SomeDealerHandFSM
mkDealerHandFSMDealing = SomeDealerHandFSM DealingFSM

mkDealerHandFSMEvaluating :: SomeDealerHandFSM
mkDealerHandFSMEvaluating = SomeDealerHandFSM EvaluatingFSM

mkDealerHandFSMResolved :: DealerHandResolution -> SomeDealerHandFSM
mkDealerHandFSMResolved res = SomeDealerHandFSM (ResolvedFSM res)

mkDealerHandFSMInterrupted :: InterruptReason -> SomeDealerHandFSM
mkDealerHandFSMInterrupted reason = SomeDealerHandFSM (InterruptedFSM reason)

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

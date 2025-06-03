{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.FSM.DealerHand where

import Data.Aeson.Types
import GHC.Generics (Generic)
import Pitboss.Blackjack
import Pitboss.FSM.Types

data DealerHandResolution
    = DHDealerBlackjack
    | DHDealerStand
    | DHDealerBust
    deriving (Eq, Show, Generic)

data DealerHandPhase
    = DHDealing
    | DHEvaluating
    | DHResolved DealerHandResolution
    | DHInterrupted InterruptReason
    deriving (Eq, Show, Generic)

instance ToJSON DealerHandResolution
instance FromJSON DealerHandResolution
instance ToJSON DealerHandPhase
instance FromJSON DealerHandPhase

data SomeDealerHandFSM = forall p. SomeDealerHandFSM (DealerHandFSM p)

data DealerHandFSM (p :: DealerHandPhase) where
    DHDealingFSM :: DealerHandFSM 'DHDealing
    DHEvaluatingFSM :: DealerHandFSM 'DHEvaluating
    DHResolvedFSM :: DealerHandResolution -> DealerHandFSM ('DHResolved r)
    DHInterruptedFSM :: InterruptReason -> DealerHandFSM ('DHInterrupted r)

deriving instance Show (DealerHandFSM p)
deriving instance Eq (DealerHandFSM p)

type family ValidDealerHandTransition (from :: DealerHandPhase) (to :: DealerHandPhase) :: Bool where
    ValidDealerHandTransition 'DHDealing 'DHEvaluating = 'True
    ValidDealerHandTransition 'DHEvaluating ('DHResolved res) = 'True
    ValidDealerHandTransition p ('DHInterrupted r) = 'True
    ValidDealerHandTransition ('DHInterrupted r) 'DHDealing = 'True
    ValidDealerHandTransition _ _ = 'False

beginEvaluationTyped ::
    (ValidDealerHandTransition 'DHDealing 'DHEvaluating ~ 'True) =>
    SomeHand ->
    DealerHandFSM 'DHDealing ->
    DealerHandFSM 'DHEvaluating
beginEvaluationTyped _hand DHDealingFSM = DHEvaluatingFSM

resolveHandTyped ::
    (ValidDealerHandTransition 'DHEvaluating ('DHResolved res) ~ 'True) =>
    SomeHand ->
    DealerHandFSM 'DHEvaluating ->
    DealerHandFSM ('DHResolved res)
resolveHandTyped (SomeHand hand) DHEvaluatingFSM = case witness hand of
    BlackjackWitness -> DHResolvedFSM DHDealerBlackjack
    BustWitness -> DHResolvedFSM DHDealerBust
    _ -> DHResolvedFSM DHDealerStand

interruptHandTyped ::
    (ValidDealerHandTransition from ('DHInterrupted r) ~ 'True) =>
    InterruptReason ->
    SomeHand ->
    DealerHandFSM from ->
    DealerHandFSM ('DHInterrupted r)
interruptHandTyped reason _hand _ = DHInterruptedFSM reason

resumeFromInterruptTyped ::
    (ValidDealerHandTransition ('DHInterrupted r) 'DHDealing ~ 'True) =>
    SomeHand ->
    DealerHandFSM ('DHInterrupted r) ->
    DealerHandFSM 'DHDealing
resumeFromInterruptTyped _hand (DHInterruptedFSM _) = DHDealingFSM

dealerShouldHit :: GameRuleSet -> SomeHand -> Bool
dealerShouldHit ruleset (SomeHand hand) = case witness hand of
    BlackjackWitness -> False
    TwentyOneWitness -> False
    BustWitness -> False
    HardWitness -> handScore (SomeHand hand) < 17
    SoftWitness ->
        let score = handScore (SomeHand hand)
         in score < 17 || score == 17 && isH17 ruleset
    PairWitness -> handScore (SomeHand hand) < 17

resolveDealerHand :: SomeHand -> DealerHandResolution
resolveDealerHand (SomeHand hand) = case witness hand of
    BlackjackWitness -> DHDealerBlackjack
    BustWitness -> DHDealerBust
    _ -> DHDealerStand

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

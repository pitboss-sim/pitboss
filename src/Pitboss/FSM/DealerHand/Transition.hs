{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Pitboss.FSM.DealerHand.Transition where

import Pitboss.Blackjack.Materia.Hand
import Pitboss.Blackjack.Offering.RuleSet (RuleSet, isH17)
import Pitboss.FSM.DealerHand.FSM
import Pitboss.FSM.DealerHand.Phase (DealerHandPhase (..), DealerHandResolution (..))
import Pitboss.FSM.Types (InterruptReason)

beginEvaluationTyped :: SomeHand -> DealerHandFSM 'Dealing -> DealerHandFSM 'Evaluating
beginEvaluationTyped _hand DealingFSM = EvaluatingFSM

resolveHandTyped :: SomeHand -> DealerHandFSM 'Evaluating -> DealerHandFSM ('Resolved res)
resolveHandTyped (SomeHand hand) EvaluatingFSM = case witness hand of
    BlackjackWitness -> ResolvedFSM DealerBlackjack
    BustWitness -> ResolvedFSM DealerBust
    _ -> ResolvedFSM DealerStand

interruptHandTyped :: InterruptReason -> SomeHand -> DealerHandFSM p -> DealerHandFSM ('Interrupted r)
interruptHandTyped reason _hand _ = InterruptedFSM reason

resumeFromInterruptTyped :: SomeHand -> DealerHandFSM ('Interrupted r) -> DealerHandFSM 'Dealing
resumeFromInterruptTyped _hand (InterruptedFSM _) = DealingFSM

dealerShouldHit :: RuleSet -> SomeHand -> Bool
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
    BlackjackWitness -> DealerBlackjack
    BustWitness -> DealerBust
    _ -> DealerStand

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Pitboss.FSM.DealerHand.Transition where

import Pitboss.Blackjack
import Pitboss.FSM.DealerHand.FSM
import Pitboss.FSM.DealerHand.Phase
import Pitboss.FSM.Types

type family ValidDealerHandTransition (from :: DealerHandPhase) (to :: DealerHandPhase) :: Bool where
    ValidDealerHandTransition 'Dealing 'Evaluating = 'True
    ValidDealerHandTransition 'Evaluating ('Resolved res) = 'True
    ValidDealerHandTransition p ('Interrupted r) = 'True
    ValidDealerHandTransition ('Interrupted r) 'Dealing = 'True
    ValidDealerHandTransition _ _ = 'False

beginEvaluationTyped ::
    (ValidDealerHandTransition 'Dealing 'Evaluating ~ 'True) =>
    SomeHand ->
    DealerHandFSM 'Dealing ->
    DealerHandFSM 'Evaluating
beginEvaluationTyped _hand DealingFSM = EvaluatingFSM

resolveHandTyped ::
    (ValidDealerHandTransition 'Evaluating ('Resolved res) ~ 'True) =>
    SomeHand ->
    DealerHandFSM 'Evaluating ->
    DealerHandFSM ('Resolved res)
resolveHandTyped (SomeHand hand) EvaluatingFSM = case witness hand of
    BlackjackWitness -> ResolvedFSM DealerBlackjack
    BustWitness -> ResolvedFSM DealerBust
    _ -> ResolvedFSM DealerStand

interruptHandTyped ::
    (ValidDealerHandTransition from ('Interrupted r) ~ 'True) =>
    InterruptReason ->
    SomeHand ->
    DealerHandFSM from ->
    DealerHandFSM ('Interrupted r)
interruptHandTyped reason _hand _ = InterruptedFSM reason

resumeFromInterruptTyped ::
    (ValidDealerHandTransition ('Interrupted r) 'Dealing ~ 'True) =>
    SomeHand ->
    DealerHandFSM ('Interrupted r) ->
    DealerHandFSM 'Dealing
resumeFromInterruptTyped _hand (InterruptedFSM _) = DealingFSM

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
    BlackjackWitness -> DealerBlackjack
    BustWitness -> DealerBust
    _ -> DealerStand

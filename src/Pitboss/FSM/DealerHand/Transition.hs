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

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.FSM.DealerHand where

import Pitboss.Blackjack.Types
import Pitboss.FSM.Types

type family ValidDealerHandTransition (from :: DealerHandPhase) (to :: DealerHandPhase) :: Bool where
    ValidDealerHandTransition 'DHAwaitingFirstCard 'DHAwaitingSecondCard = 'True
    ValidDealerHandTransition 'DHAwaitingSecondCard 'DHDealing = 'True
    ValidDealerHandTransition 'DHAwaitingSecondCard ('DHResolved res) = 'True
    ValidDealerHandTransition 'DHDealing 'DHEvaluating = 'True
    ValidDealerHandTransition 'DHEvaluating ('DHResolved res) = 'True
    ValidDealerHandTransition p ('DHInterrupted r) = 'True
    ValidDealerHandTransition ('DHInterrupted r) 'DHDealing = 'True
    ValidDealerHandTransition _ _ = 'False

receiveFirstCard ::
    SomeHand ->
    DealerHandFSM 'DHAwaitingFirstCard ->
    DealerHandFSM 'DHAwaitingSecondCard
receiveFirstCard _hand DHAwaitingFirstCardFSM = DHAwaitingSecondCardFSM

receiveSecondCard ::
    SomeHand ->
    DealerHandFSM 'DHAwaitingSecondCard ->
    DealerHandFSM 'DHDealing
receiveSecondCard _hand DHAwaitingSecondCardFSM = DHDealingFSM

receiveSecondCardBlackjack ::
    SomeHand ->
    DealerHandFSM 'DHAwaitingSecondCard ->
    DealerHandFSM ('DHResolved res)
receiveSecondCardBlackjack hand DHAwaitingSecondCardFSM =
    let witness = handWitness hand
     in case valueType witness of
            BlackjackWitness -> DHResolvedFSM DHDealerBlackjack
            _ -> error "receiveSecondCardBlackjack called on non-blackjack hand"

beginEvaluation ::
    SomeHand ->
    DealerHandFSM 'DHDealing ->
    DealerHandFSM 'DHEvaluating
beginEvaluation _hand DHDealingFSM = DHEvaluatingFSM

resolveHand ::
    SomeHand ->
    DealerHandFSM 'DHEvaluating ->
    DealerHandFSM ('DHResolved res)
resolveHand hand DHEvaluatingFSM =
    let witness = handWitness hand
     in case valueType witness of
            BlackjackWitness -> DHResolvedFSM DHDealerBlackjack
            BustWitness -> DHResolvedFSM DHDealerBust
            _ -> DHResolvedFSM DHDealerStand

interruptHand ::
    InterruptReason ->
    SomeHand ->
    DealerHandFSM from ->
    DealerHandFSM ('DHInterrupted r)
interruptHand reason _hand _ = DHInterruptedFSM reason

dealerHandResumeFromInterrupt ::
    SomeHand ->
    DealerHandFSM ('DHInterrupted r) ->
    DealerHandFSM 'DHDealing
dealerHandResumeFromInterrupt _hand (DHInterruptedFSM _) = DHDealingFSM

resolveDealerHand :: SomeHand -> DealerHandResolution
resolveDealerHand hand =
    let witness = handWitness hand
     in case valueType witness of
            BlackjackWitness -> DHDealerBlackjack
            BustWitness -> DHDealerBust
            _ -> DHDealerStand

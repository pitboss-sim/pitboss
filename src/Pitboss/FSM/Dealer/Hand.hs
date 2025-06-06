{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.FSM.Dealer.Hand where

import Pitboss.Blackjack.Types
import Pitboss.FSM.Types

type family ValidDealerHandTransition (from :: DealerHandPhase) (to :: DealerHandPhase) :: Bool where
    ValidDealerHandTransition 'DHDealing 'DHEvaluating = 'True
    ValidDealerHandTransition 'DHEvaluating ('DHResolved res) = 'True
    ValidDealerHandTransition p ('DHInterrupted r) = 'True
    ValidDealerHandTransition ('DHInterrupted r) 'DHDealing = 'True
    ValidDealerHandTransition _ _ = 'False

beginEvaluation ::
    (ValidDealerHandTransition 'DHDealing 'DHEvaluating ~ 'True) =>
    SomeHand ->
    DealerHandFSM 'DHDealing ->
    DealerHandFSM 'DHEvaluating
beginEvaluation _hand DHDealingFSM = DHEvaluatingFSM

resolveHand ::
    (ValidDealerHandTransition 'DHEvaluating ('DHResolved res) ~ 'True) =>
    SomeHand ->
    DealerHandFSM 'DHEvaluating ->
    DealerHandFSM ('DHResolved res)
resolveHand hand DHEvaluatingFSM =
    let witness = handWitness hand
     in case valueType witness of
            BlackjackWitness -> DHResolvedFSM DDealerBlackjack
            BustWitness -> DHResolvedFSM DDealerBust
            _ -> DHResolvedFSM DDealerStand

interruptHand ::
    (ValidDealerHandTransition from ('DHInterrupted r) ~ 'True) =>
    InterruptReason ->
    SomeHand ->
    DealerHandFSM from ->
    DealerHandFSM ('DHInterrupted r)
interruptHand reason _hand _ = DHInterruptedFSM reason

dealerHandResumeFromInterrupt ::
    (ValidDealerHandTransition ('DHInterrupted r) 'DHDealing ~ 'True) =>
    SomeHand ->
    DealerHandFSM ('DHInterrupted r) ->
    DealerHandFSM 'DHDealing
dealerHandResumeFromInterrupt _hand (DHInterruptedFSM _) = DHDealingFSM

resolveDealerHand :: SomeHand -> DealerHandResolution
resolveDealerHand hand =
    let witness = handWitness hand
     in case valueType witness of
            BlackjackWitness -> DDealerBlackjack
            BustWitness -> DDealerBust
            _ -> DDealerStand

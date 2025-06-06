{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.FSM.Round.Peek where

import Pitboss.Blackjack
import Pitboss.FSM.Types

type family ValidPeekTransition (from :: PeekPhase) (to :: PeekPhase) :: Bool where
    ValidPeekTransition 'PeekAwaiting 'PeekBets = 'True
    ValidPeekTransition 'PeekBets 'PeekDeal = 'True
    ValidPeekTransition 'PeekDeal 'PeekEarlySurrender = 'True
    ValidPeekTransition 'PeekDeal 'PeekPeek = 'True
    ValidPeekTransition 'PeekEarlySurrender 'PeekPeek = 'True
    ValidPeekTransition 'PeekEarlySurrender 'PeekComplete = 'True
    ValidPeekTransition 'PeekPeek 'PeekInsuranceDecision = 'True
    ValidPeekTransition 'PeekPeek 'PeekComplete = 'True
    ValidPeekTransition 'PeekInsuranceDecision 'PeekInsuranceSettled = 'True
    ValidPeekTransition 'PeekInsuranceSettled 'PeekBoutPlayers = 'True
    ValidPeekTransition 'PeekBoutPlayers 'PeekDealing = 'True
    ValidPeekTransition 'PeekDealing 'PeekSettle = 'True
    ValidPeekTransition 'PeekSettle 'PeekComplete = 'True
    ValidPeekTransition p 'PeekInterrupted = 'True
    ValidPeekTransition 'PeekInterrupted p = 'True
    ValidPeekTransition _ _ = 'False

beginPeek ::
    PeekFSM 'PeekAwaiting ->
    PeekFSM 'PeekBets
beginPeek PeekAwaitingFSM = PeekBetsFSM

betsPlacedPeek ::
    PeekFSM 'PeekBets ->
    PeekFSM 'PeekDeal
betsPlacedPeek PeekBetsFSM = PeekDealFSM

dealCardsPeek ::
    PeekFSM 'PeekDeal ->
    PeekFSM 'PeekEarlySurrender
dealCardsPeek PeekDealFSM = PeekEarlySurrenderFSM

earlySurrenderBlackjackPeek ::
    PeekFSM 'PeekEarlySurrender ->
    PeekFSM 'PeekComplete
earlySurrenderBlackjackPeek PeekEarlySurrenderFSM = PeekCompleteFSM

resolveEarlySurrenderPeek ::
    PeekFSM 'PeekEarlySurrender ->
    PeekFSM 'PeekPeek
resolveEarlySurrenderPeek PeekEarlySurrenderFSM = PeekPeekFSM

dealerBlackjackPeek ::
    PeekFSM 'PeekPeek ->
    PeekFSM 'PeekComplete
dealerBlackjackPeek PeekPeekFSM = PeekCompleteFSM

dealerNoBlackjackPeek ::
    PeekFSM 'PeekPeek ->
    PeekFSM 'PeekInsuranceDecision
dealerNoBlackjackPeek PeekPeekFSM = PeekInsuranceDecisionFSM

maybeEnterEarlySurrenderPeek ::
    GameRuleSet ->
    PeekFSM 'PeekDeal ->
    Either (PeekFSM 'PeekPeek) (PeekFSM 'PeekEarlySurrender)
maybeEnterEarlySurrenderPeek rules fsm =
    case surrender rules of
        Early -> Right (dealCardsPeek fsm)
        _ -> Left PeekPeekFSM

insuranceDecidedPeek ::
    PeekFSM 'PeekInsuranceDecision ->
    PeekFSM 'PeekInsuranceSettled
insuranceDecidedPeek PeekInsuranceDecisionFSM = PeekInsuranceSettledFSM

proceedToPlayersPeek ::
    PeekFSM 'PeekInsuranceSettled ->
    PeekFSM 'PeekBoutPlayers
proceedToPlayersPeek PeekInsuranceSettledFSM = PeekBoutPlayersFSM

finishPlayersPeek ::
    PeekFSM 'PeekBoutPlayers ->
    PeekFSM 'PeekDealing
finishPlayersPeek PeekBoutPlayersFSM = PeekDealingFSM

finishDealerPeek ::
    PeekFSM 'PeekDealing ->
    PeekFSM 'PeekSettle
finishDealerPeek PeekDealingFSM = PeekSettleFSM

resolvePayoutsPeek ::
    PeekFSM 'PeekSettle ->
    PeekFSM 'PeekComplete
resolvePayoutsPeek PeekSettleFSM = PeekCompleteFSM

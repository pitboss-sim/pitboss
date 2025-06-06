{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.FSM.Round.Peek where

import Pitboss.Blackjack
import Pitboss.FSM.Types.Round

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
    ValidPeekTransition 'PeekInsuranceSettled 'PeekContestants = 'True
    ValidPeekTransition 'PeekContestants 'PeekDealing = 'True
    ValidPeekTransition 'PeekDealing 'PeekSettle = 'True
    ValidPeekTransition 'PeekSettle 'PeekComplete = 'True
    ValidPeekTransition p 'PeekInterrupted = 'True
    ValidPeekTransition 'PeekInterrupted p = 'True
    ValidPeekTransition _ _ = 'False

beginPeek ::
    (ValidPeekTransition 'PeekAwaiting 'PeekBets ~ 'True) =>
    PeekFSM 'PeekAwaiting ->
    PeekFSM 'PeekBets
beginPeek PeekAwaitingFSM = PeekBetsFSM

betsPlacedPeek ::
    (ValidPeekTransition 'PeekBets 'PeekDeal ~ 'True) =>
    PeekFSM 'PeekBets ->
    PeekFSM 'PeekDeal
betsPlacedPeek PeekBetsFSM = PeekDealFSM

dealCardsPeek ::
    (ValidPeekTransition 'PeekDeal 'PeekEarlySurrender ~ 'True) =>
    PeekFSM 'PeekDeal ->
    PeekFSM 'PeekEarlySurrender
dealCardsPeek PeekDealFSM = PeekEarlySurrenderFSM

earlySurrenderBlackjackPeek ::
    (ValidPeekTransition 'PeekEarlySurrender 'PeekComplete ~ 'True) =>
    PeekFSM 'PeekEarlySurrender ->
    PeekFSM 'PeekComplete
earlySurrenderBlackjackPeek PeekEarlySurrenderFSM = PeekCompleteFSM

resolveEarlySurrenderPeek ::
    (ValidPeekTransition 'PeekEarlySurrender 'PeekPeek ~ 'True) =>
    PeekFSM 'PeekEarlySurrender ->
    PeekFSM 'PeekPeek
resolveEarlySurrenderPeek PeekEarlySurrenderFSM = PeekPeekFSM

dealerBlackjackPeek ::
    (ValidPeekTransition 'PeekPeek 'PeekComplete ~ 'True) =>
    PeekFSM 'PeekPeek ->
    PeekFSM 'PeekComplete
dealerBlackjackPeek PeekPeekFSM = PeekCompleteFSM

dealerNoBlackjackPeek ::
    (ValidPeekTransition 'PeekPeek 'PeekInsuranceDecision ~ 'True) =>
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
    (ValidPeekTransition 'PeekInsuranceDecision 'PeekInsuranceSettled ~ 'True) =>
    PeekFSM 'PeekInsuranceDecision ->
    PeekFSM 'PeekInsuranceSettled
insuranceDecidedPeek PeekInsuranceDecisionFSM = PeekInsuranceSettledFSM

proceedToPlayersPeek ::
    (ValidPeekTransition 'PeekInsuranceSettled 'PeekContestants ~ 'True) =>
    PeekFSM 'PeekInsuranceSettled ->
    PeekFSM 'PeekContestants
proceedToPlayersPeek PeekInsuranceSettledFSM = PeekContestantsFSM

finishPlayersPeek ::
    (ValidPeekTransition 'PeekContestants 'PeekDealing ~ 'True) =>
    PeekFSM 'PeekContestants ->
    PeekFSM 'PeekDealing
finishPlayersPeek PeekContestantsFSM = PeekDealingFSM

finishDealerPeek ::
    (ValidPeekTransition 'PeekDealing 'PeekSettle ~ 'True) =>
    PeekFSM 'PeekDealing ->
    PeekFSM 'PeekSettle
finishDealerPeek PeekDealingFSM = PeekSettleFSM

resolvePayoutsPeek ::
    (ValidPeekTransition 'PeekSettle 'PeekComplete ~ 'True) =>
    PeekFSM 'PeekSettle ->
    PeekFSM 'PeekComplete
resolvePayoutsPeek PeekSettleFSM = PeekCompleteFSM

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Pitboss.Mechanics.Dealer.Transitions where

import Pitboss.Blackjack.Hand
import Pitboss.Blackjack.Hand.Score hiding (Blackjack)
import Pitboss.Blackjack.Offering.RuleSet hiding (Peek)
import Pitboss.Mechanics.Dealer.Types.DealerRoundFSM
import Pitboss.Mechanics.Player.Types hiding (DealerBlackjack)
import Pitboss.Mechanics.Types.PhaseTag

-- hand fsm
-- TBD

-- dealer round fsm

roundFlavor :: DealerRoundFSM -> DealerRoundFlavor
roundFlavor = \case
  PeekDealerRound _ -> IsPeek
  ENHCDealerRound _ -> IsENHC

roundPhase :: DealerRoundFSM -> DealerRoundPhase
roundPhase = \case
  PeekDealerRound (SomePeekFSM f) -> case f of
    PeekAwaitingFSM -> Awaiting
    PeekBetsFSM -> Bets
    PeekDealFSM -> Deal
    PeekEarlySurrenderFSM -> EarlySurrender
    PeekPeekFSM -> Peek
    PeekInsuranceDecisionFSM -> InsuranceDecision
    PeekInsuranceSettledFSM -> InsuranceSettled
    PeekPlayersFSM -> Players
    PeekDealerFSM -> Dealer
    PeekSettleFSM -> Settle
    PeekCompleteFSM -> Complete
  ENHCDealerRound (SomeENHCFSM f) -> case f of
    ENHCAwaitingFSM -> Awaiting
    ENHCBetsFSM -> Bets
    ENHCDealFSM -> Deal
    ENHCEarlySurrenderFSM -> EarlySurrender
    ENHCPlayersFSM -> Players
    ENHCDealerFSM -> Dealer
    ENHCSettleFSM -> Settle
    ENHCCompleteFSM -> Complete

abandonHandDueToSurrender :: RuleSet -> Bool -> SomePlayerHandFSM
abandonHandDueToSurrender _ early =
  if early
    then mkPlayerHandFSMAbandoned (Surrender Early)
    else mkPlayerHandFSMAbandoned (Surrender Late)

abandonHandDueToInsurance :: Bool -> SomePlayerHandFSM
abandonHandDueToInsurance evenMoney =
  mkPlayerHandFSMAbandoned $
    if evenMoney then Insurance PaidEvenMoney else Insurance Paid

maybeEnterEarlySurrenderPeek ::
  RuleSet ->
  PeekFSM 'PeekDeal ->
  Either (PeekFSM 'PeekPeek) (PeekFSM 'PeekEarlySurrender)
maybeEnterEarlySurrenderPeek rules fsm =
  case surrender rules of
    Early -> Right (dealCardsPeek fsm)
    _ -> Left PeekPeekFSM

maybeEnterEarlySurrenderENHC ::
  RuleSet ->
  ENHCFSM 'ENHCDeal ->
  Either (ENHCFSM 'ENHCPlayers) (ENHCFSM 'ENHCEarlySurrender)
maybeEnterEarlySurrenderENHC rules fsm =
  case surrender rules of
    Early -> Right (dealCardsENHC fsm)
    _ -> Left ENHCPlayersFSM

phaseTagDealerRoundFSM :: DealerRoundFSM -> DealerRoundPhase
phaseTagDealerRoundFSM = \case
  PeekDealerRound (SomePeekFSM f) -> phaseTag f
  ENHCDealerRound (SomeENHCFSM f) -> phaseTag f

atPlayersPhase :: DealerRoundFSM -> Bool
atPlayersPhase = \case
  PeekDealerRound (SomePeekFSM _) -> True
  ENHCDealerRound (SomeENHCFSM _) -> True

-- dealer round fsm (ENHC)

beginENHC :: ENHCFSM 'ENHCAwaiting -> ENHCFSM 'ENHCBets
beginENHC ENHCAwaitingFSM = ENHCBetsFSM

betsPlacedENHC :: ENHCFSM 'ENHCBets -> ENHCFSM 'ENHCDeal
betsPlacedENHC ENHCBetsFSM = ENHCDealFSM

dealCardsENHC :: ENHCFSM 'ENHCDeal -> ENHCFSM 'ENHCEarlySurrender
dealCardsENHC ENHCDealFSM = ENHCEarlySurrenderFSM

insuranceDecidedENHC :: ENHCFSM 'ENHCEarlySurrender -> ENHCFSM 'ENHCPlayers
insuranceDecidedENHC = resolveEarlySurrenderENHC -- shim, for uniformity

resolveEarlySurrenderENHC :: ENHCFSM 'ENHCEarlySurrender -> ENHCFSM 'ENHCPlayers
resolveEarlySurrenderENHC ENHCEarlySurrenderFSM = ENHCPlayersFSM

finishPlayersENHC :: ENHCFSM 'ENHCPlayers -> ENHCFSM 'ENHCDealer
finishPlayersENHC ENHCPlayersFSM = ENHCDealerFSM

finishDealerENHC :: ENHCFSM 'ENHCDealer -> ENHCFSM 'ENHCSettle
finishDealerENHC ENHCDealerFSM = ENHCSettleFSM

resolvePayoutsENHC :: ENHCFSM 'ENHCSettle -> ENHCFSM 'ENHCComplete
resolvePayoutsENHC ENHCSettleFSM = ENHCCompleteFSM

-- dealer round fsm (Peek)

beginPeek :: PeekFSM 'PeekAwaiting -> PeekFSM 'PeekBets
beginPeek PeekAwaitingFSM = PeekBetsFSM

betsPlacedPeek :: PeekFSM 'PeekBets -> PeekFSM 'PeekDeal
betsPlacedPeek PeekBetsFSM = PeekDealFSM

dealCardsPeek :: PeekFSM 'PeekDeal -> PeekFSM 'PeekEarlySurrender
dealCardsPeek PeekDealFSM = PeekEarlySurrenderFSM

earlySurrenderBlackjackPeek :: PeekFSM 'PeekEarlySurrender -> PeekFSM 'PeekComplete
earlySurrenderBlackjackPeek PeekEarlySurrenderFSM = PeekCompleteFSM

resolveEarlySurrenderPeek :: PeekFSM 'PeekEarlySurrender -> PeekFSM 'PeekPeek
resolveEarlySurrenderPeek PeekEarlySurrenderFSM = PeekPeekFSM

dealerBlackjackPeek :: PeekFSM 'PeekPeek -> PeekFSM 'PeekComplete
dealerBlackjackPeek PeekPeekFSM = PeekCompleteFSM

dealerNoBlackjackPeek :: PeekFSM 'PeekPeek -> PeekFSM 'PeekInsuranceDecision
dealerNoBlackjackPeek PeekPeekFSM = PeekInsuranceDecisionFSM

insuranceDecidedPeek :: PeekFSM 'PeekInsuranceDecision -> PeekFSM 'PeekInsuranceSettled
insuranceDecidedPeek PeekInsuranceDecisionFSM = PeekInsuranceSettledFSM

proceedToPlayersPeek :: PeekFSM 'PeekInsuranceSettled -> PeekFSM 'PeekPlayers
proceedToPlayersPeek PeekInsuranceSettledFSM = PeekPlayersFSM

finishPlayersPeek :: PeekFSM 'PeekPlayers -> PeekFSM 'PeekDealer
finishPlayersPeek PeekPlayersFSM = PeekDealerFSM

finishDealerPeek :: PeekFSM 'PeekDealer -> PeekFSM 'PeekSettle
finishDealerPeek PeekDealerFSM = PeekSettleFSM

resolvePayoutsPeek :: PeekFSM 'PeekSettle -> PeekFSM 'PeekComplete
resolvePayoutsPeek PeekSettleFSM = PeekCompleteFSM

-- dealer hand fsm

dealerShouldHit :: RuleSet -> Hand -> Bool
dealerShouldHit ruleset hand = case dealerHandTotal hand of
  Nothing -> False -- busted
  Just DealerBlackjack -> False
  Just (Total n soft) -> n < 17 || (n == 17 && soft && isH17 ruleset)

resolveDealerHand :: Hand -> HandResolution
resolveDealerHand hand = case dealerHandTotal hand of
  Nothing -> Bust
  Just DealerBlackjack -> Blackjack
  Just _ -> Stand

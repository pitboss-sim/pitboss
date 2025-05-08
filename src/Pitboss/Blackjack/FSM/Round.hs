{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}

module Pitboss.Blackjack.FSM.Round where

import Pitboss.Blackjack.FSM.Hand (AbandonedReason (..), SomeHandFSM, mkHandFSMAbandoned)
import Pitboss.Blackjack.FSM.Round.ENHC (ENHCFSM (..), ENHCPhase (..), dealCardsENHC)
import Pitboss.Blackjack.FSM.Round.Peek (PeekFSM (..), PeekPhase (..), dealCardsPeek)
import Pitboss.Blackjack.FSM.Types.Transitionable (Transitionable (..))
import Pitboss.Blackjack.Offering.RuleSet (InsuranceOutcome (..), RuleSet (..), Surrender (..))

data RoundFlavor = IsPeek | IsENHC

roundFlavor :: RoundFSM -> RoundFlavor
roundFlavor = \case
  PeekRound _ -> IsPeek
  ENHCRound _ -> IsENHC

data RoundFSM where
  PeekRound :: PeekFSM p -> RoundFSM
  ENHCRound :: ENHCFSM p -> RoundFSM

data RoundPhase
  = Awaiting
  | Bets
  | Deal
  | EarlySurrender
  | InsuranceDecision
  | InsuranceSettled
  | Peek
  | Players
  | Dealer
  | Settle
  | Complete

roundPhase :: RoundFSM -> RoundPhase
roundPhase = \case
  PeekRound f -> case f of
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
  ENHCRound f -> case f of
    ENHCAwaitingFSM -> Awaiting
    ENHCBetsFSM -> Bets
    ENHCDealFSM -> Deal
    ENHCEarlySurrenderFSM -> EarlySurrender
    ENHCPlayersFSM -> Players
    ENHCDealerFSM -> Dealer
    ENHCSettleFSM -> Settle
    ENHCCompleteFSM -> Complete

abandonHandDueToSurrender :: RuleSet -> Bool -> SomeHandFSM
abandonHandDueToSurrender _ early =
  if early
    then mkHandFSMAbandoned (Surrender Early)
    else mkHandFSMAbandoned (Surrender Late)

abandonHandDueToInsurance :: Bool -> SomeHandFSM
abandonHandDueToInsurance evenMoney =
  mkHandFSMAbandoned $
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

class AtDecisionPoint fsm where
  toPlayersPhase :: fsm -> Maybe fsm

instance AtDecisionPoint (PeekFSM p) where
  toPlayersPhase = \case
    PeekPlayersFSM -> Just PeekPlayersFSM
    _ -> Nothing

instance AtDecisionPoint (ENHCFSM p) where
  toPlayersPhase = \case
    ENHCPlayersFSM -> Just ENHCPlayersFSM
    _ -> Nothing

atPlayersPhase :: RoundFSM -> Bool
atPlayersPhase = \case
  PeekRound PeekPlayersFSM -> True
  ENHCRound ENHCPlayersFSM -> True
  _ -> False

instance Transitionable RoundFSM where
  transitionType = \case
    PeekRound f -> transitionType f
    ENHCRound f -> transitionType f

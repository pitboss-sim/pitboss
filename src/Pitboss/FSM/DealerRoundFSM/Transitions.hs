{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Pitboss.FSM.DealerRoundFSM.Transitions where

import Pitboss.Blackjack.Offering.RuleSet
import Pitboss.FSM.DealerRoundFSM.Existential
import Pitboss.FSM.DealerRoundFSM.Types
import Pitboss.FSM.PlayerHandFSM

-- some dealer round fsm

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

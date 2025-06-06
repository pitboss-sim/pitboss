{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Pitboss.FSM.Round.PhaseTag where

import Pitboss.Blackjack.Types
import Pitboss.FSM.Types

class PhaseTag fsm phase | fsm -> phase where
    phaseTag :: fsm p -> phase

instance PhaseTag ENHCFSM RoundPhase where
    phaseTag = \case
        ENHCAwaitingFSM -> Awaiting
        ENHCBetsFSM -> Bets
        ENHCDealFSM -> Deal
        ENHCEarlySurrenderFSM -> EarlySurrender
        ENHCPlayersFSM -> Players
        ENHCDealingFSM -> Dealing
        ENHCSettleFSM -> Settle
        ENHCCompleteFSM -> Complete
        ENHCInterruptedFSM r -> Interrupted r

instance PhaseTag PeekFSM RoundPhase where
    phaseTag = \case
        PeekAwaitingFSM -> Awaiting
        PeekBetsFSM -> Bets
        PeekDealFSM -> Deal
        PeekEarlySurrenderFSM -> EarlySurrender
        PeekPeekFSM -> DealerPeek
        PeekInsuranceDecisionFSM -> InsuranceDecision
        PeekInsuranceSettledFSM -> InsuranceSettled
        PeekBoutPlayersFSM -> Players
        PeekDealingFSM -> Dealing
        PeekSettleFSM -> Settle
        PeekCompleteFSM -> Complete
        PeekInterruptedFSM r -> Interrupted r

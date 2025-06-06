{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Pitboss.FSM.Dealer.Round.PhaseTag where

import Pitboss.FSM.Dealer.Round

class PhaseTag fsm phase | fsm -> phase where
    phaseTag :: fsm p -> phase

instance PhaseTag ENHCFSM DealerRoundPhase where
    phaseTag = \case
        ENHCAwaitingFSM -> DRAwaiting
        ENHCBetsFSM -> DRBets
        ENHCDealFSM -> DRDeal
        ENHCEarlySurrenderFSM -> DREarlySurrender
        ENHCPlayersFSM -> DRPlayers
        ENHCDealingFSM -> DRDealing
        ENHCSettleFSM -> DRSettle
        ENHCCompleteFSM -> DRComplete
        ENHCInterruptedFSM r -> DRInterrupted r

instance PhaseTag PeekFSM DealerRoundPhase where
    phaseTag = \case
        PeekAwaitingFSM -> DRAwaiting
        PeekBetsFSM -> DRBets
        PeekDealFSM -> DRDeal
        PeekEarlySurrenderFSM -> DREarlySurrender
        PeekPeekFSM -> DRPeek
        PeekInsuranceDecisionFSM -> DRInsuranceDecision
        PeekInsuranceSettledFSM -> DRInsuranceSettled
        PeekPlayersFSM -> DRPlayers
        PeekDealingFSM -> DRDealing
        PeekSettleFSM -> DRSettle
        PeekCompleteFSM -> DRComplete
        PeekInterruptedFSM r -> DRInterrupted r

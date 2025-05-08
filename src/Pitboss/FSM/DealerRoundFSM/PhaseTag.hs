{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Pitboss.FSM.DealerRoundFSM.PhaseTag where

import Pitboss.FSM.DealerRoundFSM.Existential
import Pitboss.FSM.DealerRoundFSM.Types

data DealerRoundPhase
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
  | Interrupted InterruptReason

phaseTagDealerRoundFSM :: DealerRoundFSM -> DealerRoundPhase
phaseTagDealerRoundFSM = \case
  PeekDealerRound (SomePeekFSM f) -> phaseTag f
  ENHCDealerRound (SomeENHCFSM f) -> phaseTag f

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
    PeekInterruptedFSM r -> Interrupted r
  ENHCDealerRound (SomeENHCFSM f) -> case f of
    ENHCAwaitingFSM -> Awaiting
    ENHCBetsFSM -> Bets
    ENHCDealFSM -> Deal
    ENHCEarlySurrenderFSM -> EarlySurrender
    ENHCPlayersFSM -> Players
    ENHCDealerFSM -> Dealer
    ENHCSettleFSM -> Settle
    ENHCCompleteFSM -> Complete
    ENHCInterruptedFSM r -> Interrupted r

class PhaseTag fsm phase | fsm -> phase where
  phaseTag :: fsm p -> phase

instance PhaseTag ENHCFSM DealerRoundPhase where
  phaseTag = \case
    ENHCAwaitingFSM -> Awaiting
    ENHCBetsFSM -> Bets
    ENHCDealFSM -> Deal
    ENHCEarlySurrenderFSM -> EarlySurrender
    ENHCPlayersFSM -> Players
    ENHCDealerFSM -> Dealer
    ENHCSettleFSM -> Settle
    ENHCCompleteFSM -> Complete
    ENHCInterruptedFSM r -> Interrupted r

instance PhaseTag PeekFSM DealerRoundPhase where
  phaseTag = \case
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
    PeekInterruptedFSM r -> Interrupted r

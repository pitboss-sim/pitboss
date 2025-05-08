{-# LANGUAGE FunctionalDependencies #-}

module Pitboss.Mechanics.Types.PhaseTag where

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

class PhaseTag fsm phase | fsm -> phase where
  phaseTag :: fsm p -> phase

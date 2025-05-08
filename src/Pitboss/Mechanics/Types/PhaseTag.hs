{-# LANGUAGE FunctionalDependencies #-}

module Pitboss.Mechanics.Types.PhaseTag where

class PhaseTag fsm phase | fsm -> phase where
  phaseTag :: fsm p -> phase

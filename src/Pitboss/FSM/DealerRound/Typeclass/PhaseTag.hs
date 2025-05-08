{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}

module Pitboss.FSM.DealerRound.Typeclass.PhaseTag where

class PhaseTag fsm phase | fsm -> phase where
    phaseTag :: fsm p -> phase

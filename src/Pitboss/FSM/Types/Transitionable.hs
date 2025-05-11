{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Pitboss.FSM.Types.Transitionable where

import Data.Kind (Type)

data TransitionPhase
  = AwaitInput
  | AutoAdvance
  | TerminalPhase
  deriving (Eq, Show)

class Transitionable (fsm :: Type) where
  transitionType :: fsm -> TransitionPhase

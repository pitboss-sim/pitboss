{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}

module Pitboss.FSM.Types.Core where

data TransitionPhase
    = AwaitInput
    | AutoAdvance
    | TerminalPhase
    deriving (Eq, Show)

module Pitboss.FSM.DealerRound.Peek.Phase where

data PeekPhase
    = PeekAwaiting
    | PeekBets
    | PeekDeal
    | PeekEarlySurrender
    | PeekPeek
    | PeekInsuranceDecision
    | PeekInsuranceSettled
    | PeekPlayers
    | PeekDealing
    | PeekSettle
    | PeekComplete
    | PeekInterrupted

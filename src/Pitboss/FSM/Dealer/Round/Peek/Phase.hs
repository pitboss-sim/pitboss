module Pitboss.FSM.Dealer.Round.Peek.Phase where

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

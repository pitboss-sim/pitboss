module Pitboss.FSM.DealerRoundFSM.Peek.Phase where

data PeekPhase
  = PeekAwaiting
  | PeekBets
  | PeekDeal
  | PeekEarlySurrender
  | PeekPeek
  | PeekInsuranceDecision
  | PeekInsuranceSettled
  | PeekPlayers
  | PeekDealer
  | PeekSettle
  | PeekComplete
  | PeekInterrupted

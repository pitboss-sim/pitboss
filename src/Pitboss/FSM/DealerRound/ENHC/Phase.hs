module Pitboss.FSM.DealerRound.ENHC.Phase where

data ENHCPhase
  = ENHCAwaiting
  | ENHCBets
  | ENHCDeal
  | ENHCEarlySurrender
  | ENHCPlayers
  | ENHCDealer
  | ENHCSettle
  | ENHCComplete
  | ENHCInterrupted

module Pitboss.FSM.DealerRoundFSM.ENHC.Phase where

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

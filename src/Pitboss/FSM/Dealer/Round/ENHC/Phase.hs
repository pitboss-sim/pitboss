module Pitboss.FSM.Dealer.Round.ENHC.Phase where

data ENHCPhase
    = ENHCAwaiting
    | ENHCBets
    | ENHCDeal
    | ENHCEarlySurrender
    | ENHCPlayers
    | ENHCDealing
    | ENHCSettle
    | ENHCComplete
    | ENHCInterrupted

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Pitboss.Blackjack.FSM.Game.ENHC where

data ENHCPhase
  = ENHCAwaiting
  | ENHCBets
  | ENHCDeal
  | ENHCPlayers
  | ENHCDealer
  | ENHCSettle
  | ENHCComplete

data ENHCFSM (p :: ENHCPhase) where
  ENHCAwaitingFSM :: ENHCFSM 'ENHCAwaiting
  ENHCBetsFSM :: ENHCFSM 'ENHCBets
  ENHCDealFSM :: ENHCFSM 'ENHCDeal
  ENHCPlayersFSM :: ENHCFSM 'ENHCPlayers
  ENHCDealerFSM :: ENHCFSM 'ENHCDealer
  ENHCSettleFSM :: ENHCFSM 'ENHCSettle
  ENHCCompleteFSM :: ENHCFSM 'ENHCComplete

deriving instance Show (ENHCFSM p)

-- advancement

beginENHC :: ENHCFSM 'ENHCAwaiting -> ENHCFSM 'ENHCBets
beginENHC ENHCAwaitingFSM = ENHCBetsFSM

betsPlacedENHC :: ENHCFSM 'ENHCBets -> ENHCFSM 'ENHCDeal
betsPlacedENHC ENHCBetsFSM = ENHCDealFSM

dealCardsENHC :: ENHCFSM 'ENHCDeal -> ENHCFSM 'ENHCPlayers
dealCardsENHC ENHCDealFSM = ENHCPlayersFSM

finishPlayersENHC :: ENHCFSM 'ENHCPlayers -> ENHCFSM 'ENHCDealer
finishPlayersENHC ENHCPlayersFSM = ENHCDealerFSM

finishDealerENHC :: ENHCFSM 'ENHCDealer -> ENHCFSM 'ENHCSettle
finishDealerENHC ENHCDealerFSM = ENHCSettleFSM

resolvePayoutsENHC :: ENHCFSM 'ENHCSettle -> ENHCFSM 'ENHCComplete
resolvePayoutsENHC ENHCSettleFSM = ENHCCompleteFSM

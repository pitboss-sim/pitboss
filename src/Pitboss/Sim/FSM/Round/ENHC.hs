{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

module Pitboss.Sim.FSM.Round.ENHC where

import Pitboss.Sim.FSM.Types.Transitionable (TransitionPhase (..), Transitionable (..))

data ENHCPhase
  = ENHCAwaiting
  | ENHCBets
  | ENHCDeal
  | ENHCEarlySurrender
  | ENHCPlayers
  | ENHCDealer
  | ENHCSettle
  | ENHCComplete

data ENHCFSM (p :: ENHCPhase) where
  ENHCAwaitingFSM :: ENHCFSM 'ENHCAwaiting
  ENHCBetsFSM :: ENHCFSM 'ENHCBets
  ENHCDealFSM :: ENHCFSM 'ENHCDeal
  ENHCEarlySurrenderFSM :: ENHCFSM 'ENHCEarlySurrender
  ENHCPlayersFSM :: ENHCFSM 'ENHCPlayers
  ENHCDealerFSM :: ENHCFSM 'ENHCDealer
  ENHCSettleFSM :: ENHCFSM 'ENHCSettle
  ENHCCompleteFSM :: ENHCFSM 'ENHCComplete

deriving instance Show (ENHCFSM p)

deriving instance Eq (ENHCFSM p)

-- advancement

beginENHC :: ENHCFSM 'ENHCAwaiting -> ENHCFSM 'ENHCBets
beginENHC ENHCAwaitingFSM = ENHCBetsFSM

betsPlacedENHC :: ENHCFSM 'ENHCBets -> ENHCFSM 'ENHCDeal
betsPlacedENHC ENHCBetsFSM = ENHCDealFSM

dealCardsENHC :: ENHCFSM 'ENHCDeal -> ENHCFSM 'ENHCEarlySurrender
dealCardsENHC ENHCDealFSM = ENHCEarlySurrenderFSM

insuranceDecidedENHC :: ENHCFSM 'ENHCEarlySurrender -> ENHCFSM 'ENHCPlayers
insuranceDecidedENHC = resolveEarlySurrenderENHC -- shim, for uniformity

resolveEarlySurrenderENHC :: ENHCFSM 'ENHCEarlySurrender -> ENHCFSM 'ENHCPlayers
resolveEarlySurrenderENHC ENHCEarlySurrenderFSM = ENHCPlayersFSM

finishPlayersENHC :: ENHCFSM 'ENHCPlayers -> ENHCFSM 'ENHCDealer
finishPlayersENHC ENHCPlayersFSM = ENHCDealerFSM

finishDealerENHC :: ENHCFSM 'ENHCDealer -> ENHCFSM 'ENHCSettle
finishDealerENHC ENHCDealerFSM = ENHCSettleFSM

resolvePayoutsENHC :: ENHCFSM 'ENHCSettle -> ENHCFSM 'ENHCComplete
resolvePayoutsENHC ENHCSettleFSM = ENHCCompleteFSM

instance Transitionable (ENHCFSM p) where
  transitionType = \case
    ENHCAwaitingFSM -> AwaitInput
    ENHCBetsFSM -> AwaitInput
    ENHCDealFSM -> AutoAdvance
    ENHCEarlySurrenderFSM -> AwaitInput
    ENHCPlayersFSM -> AwaitInput
    ENHCDealerFSM -> AutoAdvance
    ENHCSettleFSM -> AutoAdvance
    ENHCCompleteFSM -> TerminalPhase

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Pitboss.FSM.DealerRoundFSM.Types where

import Pitboss.FSM.Types.Transitionable

data DealerRoundFlavor = IsPeek | IsENHC

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

data PeekFSM (p :: PeekPhase) where
  PeekAwaitingFSM :: PeekFSM 'PeekAwaiting
  PeekBetsFSM :: PeekFSM 'PeekBets
  PeekDealFSM :: PeekFSM 'PeekDeal
  PeekEarlySurrenderFSM :: PeekFSM 'PeekEarlySurrender
  PeekPeekFSM :: PeekFSM 'PeekPeek
  PeekInsuranceDecisionFSM :: PeekFSM 'PeekInsuranceDecision
  PeekInsuranceSettledFSM :: PeekFSM 'PeekInsuranceSettled
  PeekPlayersFSM :: PeekFSM 'PeekPlayers
  PeekDealerFSM :: PeekFSM 'PeekDealer
  PeekSettleFSM :: PeekFSM 'PeekSettle
  PeekCompleteFSM :: PeekFSM 'PeekComplete

-- Eq

deriving instance Eq (ENHCFSM p)

deriving instance Eq (PeekFSM p)

-- Show

deriving instance Show (ENHCFSM p)

deriving instance Show (PeekFSM p)

-- Transitionable

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

instance Transitionable (PeekFSM p) where
  transitionType = \case
    PeekAwaitingFSM -> AwaitInput
    PeekBetsFSM -> AwaitInput
    PeekDealFSM -> AutoAdvance
    PeekEarlySurrenderFSM -> AwaitInput
    PeekPeekFSM -> AwaitInput
    PeekInsuranceDecisionFSM -> AwaitInput
    PeekInsuranceSettledFSM -> AutoAdvance
    PeekPlayersFSM -> AwaitInput
    PeekDealerFSM -> AutoAdvance
    PeekSettleFSM -> AutoAdvance
    PeekCompleteFSM -> TerminalPhase

-- AtDecisionPoint (experimental)

class AtDecisionPoint fsm where
  toPlayersPhase :: fsm -> Maybe fsm

instance AtDecisionPoint (PeekFSM p) where
  toPlayersPhase = \case
    PeekPlayersFSM -> Just PeekPlayersFSM
    _ -> Nothing

instance AtDecisionPoint (ENHCFSM p) where
  toPlayersPhase = \case
    ENHCPlayersFSM -> Just ENHCPlayersFSM
    _ -> Nothing

-- helpers

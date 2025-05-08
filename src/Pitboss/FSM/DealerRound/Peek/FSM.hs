{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Pitboss.FSM.DealerRound.Peek.FSM where

import Pitboss.FSM.DealerRound.Peek.Phase
import Pitboss.FSM.Types

data PeekFSM (p :: PeekPhase) where
    PeekAwaitingFSM :: PeekFSM 'PeekAwaiting
    PeekBetsFSM :: PeekFSM 'PeekBets
    PeekDealFSM :: PeekFSM 'PeekDeal
    PeekEarlySurrenderFSM :: PeekFSM 'PeekEarlySurrender
    PeekPeekFSM :: PeekFSM 'PeekPeek
    PeekInsuranceDecisionFSM :: PeekFSM 'PeekInsuranceDecision
    PeekInsuranceSettledFSM :: PeekFSM 'PeekInsuranceSettled
    PeekPlayersFSM :: PeekFSM 'PeekPlayers
    PeekDealingFSM :: PeekFSM 'PeekDealing
    PeekSettleFSM :: PeekFSM 'PeekSettle
    PeekCompleteFSM :: PeekFSM 'PeekComplete
    PeekInterruptedFSM :: InterruptReason -> PeekFSM 'PeekInterrupted

deriving instance Eq (PeekFSM p)
deriving instance Show (PeekFSM p)

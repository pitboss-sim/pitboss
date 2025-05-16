{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Pitboss.FSM.DealerRound.Peek.FSM where

import Pitboss.FSM.DealerRound.Peek.Phase
import Pitboss.FSM.DealerRound.Phase
import Pitboss.FSM.DealerRound.Typeclass.AtDecisionPoint
import Pitboss.FSM.DealerRound.Typeclass.PhaseTag
import Pitboss.FSM.Types.Transitionable

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
  PeekInterruptedFSM :: InterruptReason -> PeekFSM 'PeekInterrupted

deriving instance Eq (PeekFSM p)

deriving instance Show (PeekFSM p)

instance AtDecisionPoint (PeekFSM p) where
  toPlayersPhase = \case
    PeekPlayersFSM -> Just PeekPlayersFSM
    _ -> Nothing

instance PhaseTag PeekFSM DealerRoundPhase where
  phaseTag = \case
    PeekAwaitingFSM -> Awaiting
    PeekBetsFSM -> Bets
    PeekDealFSM -> Deal
    PeekEarlySurrenderFSM -> EarlySurrender
    PeekPeekFSM -> Peek
    PeekInsuranceDecisionFSM -> InsuranceDecision
    PeekInsuranceSettledFSM -> InsuranceSettled
    PeekPlayersFSM -> Players
    PeekDealerFSM -> Dealer
    PeekSettleFSM -> Settle
    PeekCompleteFSM -> Complete
    PeekInterruptedFSM r -> Interrupted r

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
    PeekInterruptedFSM _ -> AwaitInput

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}

module Pitboss.Blackjack.FSM.Round.Peek where

import Pitboss.Blackjack.FSM.Types.Transitionable (TransitionPhase (..), Transitionable (..))

data PeekPhase
  = PeekAwaiting
  | PeekBets
  | PeekDeal
  | PeekEarlySurrender
  | PeekPeek
  | PeekInsuranceDecision
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
  PeekPlayersFSM :: PeekFSM 'PeekPlayers
  PeekDealerFSM :: PeekFSM 'PeekDealer
  PeekSettleFSM :: PeekFSM 'PeekSettle
  PeekCompleteFSM :: PeekFSM 'PeekComplete

deriving instance Show (PeekFSM p)

deriving instance Eq (PeekFSM p)

-- advancement

beginPeek :: PeekFSM 'PeekAwaiting -> PeekFSM 'PeekBets
beginPeek PeekAwaitingFSM = PeekBetsFSM

betsPlacedPeek :: PeekFSM 'PeekBets -> PeekFSM 'PeekDeal
betsPlacedPeek PeekBetsFSM = PeekDealFSM

dealCardsPeek :: PeekFSM 'PeekDeal -> PeekFSM 'PeekEarlySurrender
dealCardsPeek PeekDealFSM = PeekEarlySurrenderFSM

resolveEarlySurrenderPeek :: PeekFSM 'PeekEarlySurrender -> PeekFSM 'PeekPeek
resolveEarlySurrenderPeek PeekEarlySurrenderFSM = PeekPeekFSM

dealerBlackjackPeek :: PeekFSM 'PeekPeek -> PeekFSM 'PeekComplete
dealerBlackjackPeek PeekPeekFSM = PeekCompleteFSM

dealerNoBlackjackPeek :: PeekFSM 'PeekPeek -> PeekFSM 'PeekInsuranceDecision
dealerNoBlackjackPeek PeekPeekFSM = PeekInsuranceDecisionFSM

insuranceDecidedPeek :: PeekFSM 'PeekInsuranceDecision -> PeekFSM 'PeekPlayers
insuranceDecidedPeek PeekInsuranceDecisionFSM = PeekPlayersFSM

finishPlayersPeek :: PeekFSM 'PeekPlayers -> PeekFSM 'PeekDealer
finishPlayersPeek PeekPlayersFSM = PeekDealerFSM

finishDealerPeek :: PeekFSM 'PeekDealer -> PeekFSM 'PeekSettle
finishDealerPeek PeekDealerFSM = PeekSettleFSM

resolvePayoutsPeek :: PeekFSM 'PeekSettle -> PeekFSM 'PeekComplete
resolvePayoutsPeek PeekSettleFSM = PeekCompleteFSM

instance Transitionable (PeekFSM p) where
  transitionType = \case
    PeekAwaitingFSM -> AwaitInput
    PeekBetsFSM -> AwaitInput
    PeekDealFSM -> AutoAdvance
    PeekEarlySurrenderFSM -> AwaitInput
    PeekPeekFSM -> AwaitInput
    PeekInsuranceDecisionFSM -> AwaitInput
    PeekPlayersFSM -> AwaitInput
    PeekDealerFSM -> AutoAdvance
    PeekSettleFSM -> AutoAdvance
    PeekCompleteFSM -> TerminalPhase

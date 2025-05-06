{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Pitboss.Blackjack.FSM.Game.Peek where

data PeekPhase
  = PeekAwaiting
  | PeekBets
  | PeekDeal
  | PeekPeek
  | PeekPlayers
  | PeekDealer
  | PeekSettle
  | PeekComplete

data PeekFSM (p :: PeekPhase) where
  PeekAwaitingFSM :: PeekFSM 'PeekAwaiting
  PeekBetsFSM :: PeekFSM 'PeekBets
  PeekDealFSM :: PeekFSM 'PeekDeal
  PeekPeekFSM :: PeekFSM 'PeekPeek
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

dealCardsPeek :: PeekFSM 'PeekDeal -> PeekFSM 'PeekPeek
dealCardsPeek PeekDealFSM = PeekPeekFSM

dealerBlackjackPeek :: PeekFSM 'PeekPeek -> PeekFSM 'PeekComplete
dealerBlackjackPeek PeekPeekFSM = PeekCompleteFSM

dealerNoBlackjackPeek :: PeekFSM 'PeekPeek -> PeekFSM 'PeekPlayers
dealerNoBlackjackPeek PeekPeekFSM = PeekPlayersFSM

finishPlayersPeek :: PeekFSM 'PeekPlayers -> PeekFSM 'PeekDealer
finishPlayersPeek PeekPlayersFSM = PeekDealerFSM

finishDealerPeek :: PeekFSM 'PeekDealer -> PeekFSM 'PeekSettle
finishDealerPeek PeekDealerFSM = PeekSettleFSM

resolvePayoutsPeek :: PeekFSM 'PeekSettle -> PeekFSM 'PeekComplete
resolvePayoutsPeek PeekSettleFSM = PeekCompleteFSM

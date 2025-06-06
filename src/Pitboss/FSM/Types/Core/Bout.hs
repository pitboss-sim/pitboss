{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Pitboss.FSM.Types.Core.Bout where

data BoutPhase
    = BAwaitingFirstCard
    | BAwaitingSecondCard
    | BPlayerTurn
    | BDealerTurn
    | BSettlement
    | BDone
    deriving (Eq, Show)

data BoutFSM (p :: BoutPhase) where
    BAwaitingFirstCardFSM :: BoutFSM 'BAwaitingFirstCard
    BAwaitingSecondCardFSM :: BoutFSM 'BAwaitingSecondCard
    BPlayerTurnFSM :: BoutFSM 'BPlayerTurn
    BDealerTurnFSM :: BoutFSM 'BDealerTurn
    BSettlementFSM :: BoutFSM 'BSettlement
    BDoneFSM :: BoutFSM 'BDone

deriving instance Show (BoutFSM p)
deriving instance Eq (BoutFSM p)

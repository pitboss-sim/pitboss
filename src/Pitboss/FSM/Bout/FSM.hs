{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Pitboss.FSM.Bout.FSM where

import Pitboss.FSM.Bout.Phase
import Pitboss.FSM.Types.Transitionable

data BoutFSM (p :: BoutPhase) where
    BAwaitingFirstCardFSM :: BoutFSM 'BAwaitingFirstCard
    BAwaitingSecondCardFSM :: BoutFSM 'BAwaitingSecondCard
    BPlayerTurnFSM :: BoutFSM 'BPlayerTurn
    BDealerTurnFSM :: BoutFSM 'BDealerTurn
    BSettlementFSM :: BoutFSM 'BSettlement
    BDoneFSM :: BoutFSM 'BDone

deriving instance Show (BoutFSM p)
deriving instance Eq (BoutFSM p)

instance Transitionable (BoutFSM p) where
    transitionType = \case
        BAwaitingFirstCardFSM -> AwaitInput
        BAwaitingSecondCardFSM -> AwaitInput
        BPlayerTurnFSM -> AwaitInput
        BDealerTurnFSM -> AutoAdvance
        BSettlementFSM -> AutoAdvance
        BDoneFSM -> TerminalPhase

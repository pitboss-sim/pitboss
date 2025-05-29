{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Pitboss.FSM.Bout.FSM where

import Pitboss.FSM.Bout.Phase
import Pitboss.FSM.Types.Transitionable


data BoutFSM (p :: BoutPhase) where
    AwaitingFirstCardFSM :: BoutFSM 'AwaitingFirstCard
    AwaitingSecondCardFSM :: BoutFSM 'AwaitingSecondCard
    PlayerTurnFSM :: BoutFSM 'PlayerTurn
    DealerTurnFSM :: BoutFSM 'DealerTurn
    SettlementFSM :: BoutFSM 'Settlement
    DoneFSM :: BoutFSM 'Done

deriving instance Show (BoutFSM p)
deriving instance Eq (BoutFSM p)

instance Transitionable (BoutFSM p) where
    transitionType = \case
        AwaitingFirstCardFSM -> AwaitInput
        AwaitingSecondCardFSM -> AwaitInput
        PlayerTurnFSM -> AwaitInput
        DealerTurnFSM -> AutoAdvance
        SettlementFSM -> AutoAdvance
        DoneFSM -> TerminalPhase

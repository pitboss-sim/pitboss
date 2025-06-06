{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}

module Pitboss.FSM.Transitionable where

import Data.Kind (Type)
import Pitboss.FSM.Types

data TransitionPhase
    = AwaitInput
    | AutoAdvance
    | TerminalPhase
    deriving (Eq, Show)

class Transitionable (fsm :: Type) where
    transitionType :: fsm -> TransitionPhase

instance Transitionable SomeBoutFSM where
    transitionType (SomeBoutFSM fsm) = transitionType fsm

instance Transitionable SomeDealerHandFSM where
    transitionType (SomeDealerHandFSM fsm) = transitionType fsm

instance Transitionable SomeDealerTableFSM where
    transitionType (SomeDealerTableFSM fsm) = transitionType fsm

instance Transitionable SomeENHCFSM where
    transitionType (SomeENHCFSM fsm) = transitionType fsm

instance Transitionable SomePeekFSM where
    transitionType (SomePeekFSM fsm) = transitionType fsm

instance Transitionable SomePlayerFSM where
    transitionType (SomePlayerFSM fsm) = transitionType fsm

instance Transitionable SomePlayerHandFSM where
    transitionType (SomePlayerHandFSM fsm) = transitionType fsm

instance Transitionable SomeTableFSM where
    transitionType (SomeTableFSM fsm) = transitionType fsm

instance Transitionable (BoutFSM p) where
    transitionType = \case
        BAwaitingFirstCardFSM -> AwaitInput
        BAwaitingSecondCardFSM -> AwaitInput
        BPlayerTurnFSM -> AwaitInput
        BDealerTurnFSM -> AutoAdvance
        BSettlementFSM -> AutoAdvance
        BDoneFSM -> TerminalPhase

instance Transitionable (DealerHandFSM p) where
    transitionType = \case
        DHAwaitingFirstCardFSM -> AwaitInput
        DHAwaitingSecondCardFSM -> AwaitInput
        DHDealingFSM -> AwaitInput
        DHEvaluatingFSM -> AutoAdvance
        DHPlayingFSM -> AutoAdvance
        DHResolvedFSM _ -> TerminalPhase
        DHInterruptedFSM _ -> AwaitInput

instance Transitionable (DealerTableFSM p) where
    transitionType = \case
        DOffDutyFSM -> AwaitInput
        DPushingFSM -> AutoAdvance
        DOnDutyFSM -> AwaitInput
        DTaskingFSM _ -> AwaitInput
        DLeavingFSM -> AutoAdvance

instance Transitionable (ENHCFSM p) where
    transitionType = \case
        ENHCAwaitingFSM -> AwaitInput
        ENHCBetsFSM -> AwaitInput
        ENHCDealFSM -> AutoAdvance
        ENHCEarlySurrenderFSM -> AwaitInput
        ENHCPlayersFSM -> AwaitInput
        ENHCDealingFSM -> AutoAdvance
        ENHCSettleFSM -> AutoAdvance
        ENHCCompleteFSM -> TerminalPhase
        ENHCInterruptedFSM _ -> AwaitInput

instance Transitionable (PeekFSM p) where
    transitionType = \case
        PeekAwaitingFSM -> AwaitInput
        PeekBetsFSM -> AwaitInput
        PeekDealFSM -> AutoAdvance
        PeekEarlySurrenderFSM -> AwaitInput
        PeekPeekFSM -> AwaitInput
        PeekInsuranceDecisionFSM -> AwaitInput
        PeekInsuranceSettledFSM -> AutoAdvance
        PeekBoutPlayersFSM -> AwaitInput
        PeekDealingFSM -> AutoAdvance
        PeekSettleFSM -> AutoAdvance
        PeekCompleteFSM -> TerminalPhase
        PeekInterruptedFSM _ -> AwaitInput

instance Transitionable (PlayerFSM p) where
    transitionType = \case
        PIdleFSM -> AwaitInput
        PChoosingTableFSM -> AwaitInput
        PPlacingBetFSM -> AwaitInput
        PPlayingHandFSM -> AwaitInput
        PObservingFSM -> AutoAdvance
        PDoneFSM -> TerminalPhase

instance Transitionable (PlayerHandFSM p h d s) where
    transitionType = \case
        PHAwaitingFirstCardFSM -> AwaitInput
        PHAwaitingSecondCardFSM -> AwaitInput
        PHDecisionFSM -> AwaitInput
        PHHittingFSM -> AutoAdvance
        PHAwaitingOneCardFSM _ -> AwaitInput
        PHResolvedFSM _ -> TerminalPhase
        PHAbandonedFSM _ -> TerminalPhase

instance Transitionable RoundFSM where
    transitionType = \case
        PeekRound f -> transitionType f
        ENHCRound f -> transitionType f

instance Transitionable (TableFSM p) where
    transitionType = \case
        TClosedFSM -> AwaitInput
        TOpeningFSM -> AutoAdvance
        TRoundInProgressFSM -> AwaitInput
        TIntermissionFSM -> AutoAdvance
        TInterruptedFSM _ -> AwaitInput
        TClosingFSM -> AutoAdvance

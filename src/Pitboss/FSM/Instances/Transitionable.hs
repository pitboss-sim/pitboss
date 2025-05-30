{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}

module Pitboss.FSM.Instances.Transitionable where

import Data.Kind (Type)
import Pitboss.FSM

data TransitionPhase
    = AwaitInput
    | AutoAdvance
    | TerminalPhase
    deriving (Eq, Show)

class Transitionable (fsm :: Type) where
    transitionType :: fsm -> TransitionPhase

instance Transitionable SomeBoutFSM where
    transitionType (SomeBoutFSM fsm) = transitionType fsm

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
        DHDealingFSM -> AwaitInput
        DHEvaluatingFSM -> AutoAdvance
        DHResolvedFSM _ -> TerminalPhase
        DHInterruptedFSM _ -> AwaitInput

instance Transitionable DealerRoundFSM where
    transitionType = \case
        PeekDealerRound f -> transitionType f
        ENHCDealerRound f -> transitionType f

instance Transitionable (DealerTableFSM p) where
    transitionType = \case
        DTOffDutyFSM -> AwaitInput
        DTPushingFSM -> AutoAdvance
        DTOnDutyFSM -> AwaitInput
        DTTaskingFSM _ -> AwaitInput
        DTLeavingFSM -> AutoAdvance

instance Transitionable SomePeekFSM where
    transitionType (SomePeekFSM fsm) = transitionType fsm

instance Transitionable SomeENHCFSM where
    transitionType (SomeENHCFSM fsm) = transitionType fsm

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
        PeekPlayersFSM -> AwaitInput
        PeekDealingFSM -> AutoAdvance
        PeekSettleFSM -> AutoAdvance
        PeekCompleteFSM -> TerminalPhase
        PeekInterruptedFSM _ -> AwaitInput

instance Transitionable (PlayerHandFSM p h d s) where
    transitionType = \case
        PHDecisionFSM -> AwaitInput
        PHHittingFSM -> AwaitInput
        PHOneCardDrawFSM _ -> AutoAdvance
        PHResolvedFSM _ -> TerminalPhase
        PHAbandonedFSM _ -> TerminalPhase
        PHBlackjackFSM -> TerminalPhase

instance Transitionable SomePlayerSpotFSM where
    transitionType (SomePlayerSpotFSM fsm) = transitionType fsm

instance Transitionable (PlayerSpotFSM p) where
    transitionType = \case
        PSIdleFSM -> AwaitInput
        PSEngagedFSM -> AwaitInput
        PSWaitingForHandsFSM -> AwaitInput
        PSResolvedFSM -> TerminalPhase
        PSInterruptedFSM _ -> AwaitInput

instance Transitionable (PlayerTableFSM p) where
    transitionType = \case
        PTIdleFSM -> AwaitInput
        PTChoosingTableFSM -> AwaitInput
        PTPlacingBetFSM -> AwaitInput
        PTPlayingHandFSM -> AwaitInput
        PTObservingFSM -> AutoAdvance
        PTDoneFSM -> TerminalPhase

instance Transitionable SomeTableFSM where
    transitionType (SomeTableFSM fsm) = transitionType fsm

instance Transitionable (TableFSM p) where
    transitionType = \case
        TClosedFSM -> AwaitInput
        TOpeningFSM -> AutoAdvance
        TRoundInProgressFSM -> AwaitInput
        TIntermissionFSM -> AutoAdvance
        TInterruptedFSM _ -> AwaitInput
        TClosingFSM -> AutoAdvance


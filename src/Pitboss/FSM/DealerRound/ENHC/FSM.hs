{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Pitboss.FSM.DealerRound.ENHC.FSM where

import Pitboss.FSM.DealerRound.ENHC.Phase
import Pitboss.FSM.DealerRound.Phase
import Pitboss.FSM.DealerRound.Typeclass.AtDecisionPoint
import Pitboss.FSM.DealerRound.Typeclass.PhaseTag
import Pitboss.FSM.Types
import Pitboss.FSM.Types.Transitionable

data ENHCFSM (p :: ENHCPhase) where
    ENHCAwaitingFSM :: ENHCFSM 'ENHCAwaiting
    ENHCBetsFSM :: ENHCFSM 'ENHCBets
    ENHCDealFSM :: ENHCFSM 'ENHCDeal
    ENHCEarlySurrenderFSM :: ENHCFSM 'ENHCEarlySurrender
    ENHCPlayersFSM :: ENHCFSM 'ENHCPlayers
    ENHCDealingFSM :: ENHCFSM 'ENHCDealing
    ENHCSettleFSM :: ENHCFSM 'ENHCSettle
    ENHCCompleteFSM :: ENHCFSM 'ENHCComplete
    ENHCInterruptedFSM :: InterruptReason -> ENHCFSM 'ENHCInterrupted

deriving instance Eq (ENHCFSM p)

deriving instance Show (ENHCFSM p)

instance AtDecisionPoint (ENHCFSM p) where
    toPlayersPhase = \case
        ENHCPlayersFSM -> Just ENHCPlayersFSM
        _ -> Nothing

instance PhaseTag ENHCFSM DealerRoundPhase where
    phaseTag = \case
        ENHCAwaitingFSM -> DRAwaiting
        ENHCBetsFSM -> DRBets
        ENHCDealFSM -> DRDeal
        ENHCEarlySurrenderFSM -> DREarlySurrender
        ENHCPlayersFSM -> DRPlayers
        ENHCDealingFSM -> DRDealing
        ENHCSettleFSM -> DRSettle
        ENHCCompleteFSM -> DRComplete
        ENHCInterruptedFSM r -> DRInterrupted r

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

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Pitboss.FSM.DealerHand.FSM where

import Pitboss.FSM.DealerHand.Phase
import Pitboss.FSM.Types
import Pitboss.FSM.Types.Transitionable

data DealerHandFSM (p :: DealerHandPhase) where
    DHDealingFSM :: DealerHandFSM 'DHDealing
    DHEvaluatingFSM :: DealerHandFSM 'DHEvaluating
    DHResolvedFSM :: DealerHandResolution -> DealerHandFSM ('DHResolved r)
    DHInterruptedFSM :: InterruptReason -> DealerHandFSM ('DHInterrupted r)

deriving instance Show (DealerHandFSM p)
deriving instance Eq (DealerHandFSM p)

instance Transitionable (DealerHandFSM p) where
    transitionType = \case
        DHDealingFSM -> AwaitInput
        DHEvaluatingFSM -> AutoAdvance
        DHResolvedFSM _ -> TerminalPhase
        DHInterruptedFSM _ -> AwaitInput

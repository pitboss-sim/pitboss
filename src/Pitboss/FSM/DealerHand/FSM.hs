{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Pitboss.FSM.DealerHand.FSM where

import Pitboss.FSM.DealerHand.Phase
import Pitboss.FSM.DealerRound.Phase hiding (Interrupted)
import Pitboss.FSM.Types.Transitionable

data DealerHandFSM (p :: DealerHandPhase) where
  DealingFSM :: DealerHandFSM 'Dealing
  EvaluatingFSM :: DealerHandFSM 'Evaluating
  ResolvedFSM :: DealerHandResolution -> DealerHandFSM ('Resolved r)
  InterruptedFSM :: InterruptReason -> DealerHandFSM ('Interrupted r)

deriving instance Show (DealerHandFSM p)

deriving instance Eq (DealerHandFSM p)

instance Transitionable (DealerHandFSM p) where
  transitionType = \case
    DealingFSM -> AwaitInput
    EvaluatingFSM -> AutoAdvance
    ResolvedFSM _ -> TerminalPhase
    InterruptedFSM _ -> AwaitInput

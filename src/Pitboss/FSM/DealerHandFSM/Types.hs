{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Pitboss.FSM.DealerHandFSM.Types where

import GHC.Generics
import Pitboss.FSM.DealerRoundFSM hiding (Interrupted)
import Pitboss.FSM.Types.Transitionable

data DealerHandResolution
  = DealerBlackjack
  | DealerStand
  | DealerBust
  deriving (Eq, Show, Generic)

data DealerHandPhase
  = Dealing
  | Evaluating
  | Resolved DealerHandResolution
  | Interrupted InterruptReason
  deriving (Eq, Show, Generic)

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

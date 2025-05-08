{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Pitboss.FSM.DealerTableFSM.Types where

import GHC.Generics
import Pitboss.FSM.Types.Transitionable
import Pitboss.Trace.Timeline.Identifier

data DealerTablePhase
  = OffDuty
  | Pushing
  | OnDuty
  | Tasking DealerTask
  | Leaving
  deriving (Eq, Show, Generic)

data DealerTask
  = Cleanup
  | Shuffle
  | AttendChips
  | RespondToPlayer PlayerId
  | RespondToEnvironment
  deriving (Eq, Show, Generic)

data DealerTableFSM (p :: DealerTablePhase) where
  OffDutyFSM :: DealerTableFSM 'OffDuty
  PushingFSM :: TableId -> DealerTableFSM 'Pushing
  OnDutyFSM :: TableId -> DealerTableFSM 'OnDuty
  TaskingFSM :: DealerTask -> DealerTableFSM ('Tasking task)
  LeavingFSM :: TableId -> DealerTableFSM 'Leaving

-- Eq

deriving instance Eq (DealerTableFSM p)

-- Show

deriving instance Show (DealerTableFSM p)

-- Transitionable

instance Transitionable (DealerTableFSM p) where
  transitionType = \case
    OffDutyFSM -> AwaitInput
    PushingFSM _ -> AutoAdvance
    OnDutyFSM _ -> AwaitInput
    TaskingFSM _ -> AwaitInput
    LeavingFSM _ -> AutoAdvance

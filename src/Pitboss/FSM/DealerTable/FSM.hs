{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Pitboss.FSM.DealerTable.FSM where

import Pitboss.FSM.DealerTable.Phase
import Pitboss.FSM.Types.Transitionable

data DealerTableFSM (p :: DealerTablePhase) where
    OffDutyFSM :: DealerTableFSM 'OffDuty
    PushingFSM :: DealerTableFSM 'Pushing
    OnDutyFSM :: DealerTableFSM 'OnDuty
    TaskingFSM :: DealerTask -> DealerTableFSM ('Tasking task)
    LeavingFSM :: DealerTableFSM 'Leaving

deriving instance Eq (DealerTableFSM p)

deriving instance Show (DealerTableFSM p)

instance Transitionable (DealerTableFSM p) where
    transitionType = \case
        OffDutyFSM -> AwaitInput
        PushingFSM -> AutoAdvance
        OnDutyFSM -> AwaitInput
        TaskingFSM _ -> AwaitInput
        LeavingFSM -> AutoAdvance

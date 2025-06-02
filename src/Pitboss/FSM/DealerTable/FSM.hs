{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Pitboss.FSM.DealerTable.FSM where

import Pitboss.FSM.DealerTable.Phase
import Pitboss.FSM.Types.Transitionable

data DealerTableFSM (p :: DealerTablePhase) where
    DTOffDutyFSM :: DealerTableFSM 'DTOffDuty
    DTPushingFSM :: DealerTableFSM 'DTPushing
    DTOnDutyFSM :: DealerTableFSM 'DTOnDuty
    DTTaskingFSM :: DealerTask -> DealerTableFSM ('DTTasking task)
    DTLeavingFSM :: DealerTableFSM 'DTLeaving

deriving instance Eq (DealerTableFSM p)

deriving instance Show (DealerTableFSM p)

instance Transitionable (DealerTableFSM p) where
    transitionType = \case
        DTOffDutyFSM -> AwaitInput
        DTPushingFSM -> AutoAdvance
        DTOnDutyFSM -> AwaitInput
        DTTaskingFSM _ -> AwaitInput
        DTLeavingFSM -> AutoAdvance

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.FSM.Types.Core.Dealer where

import Data.Aeson.Types
import GHC.Generics (Generic)

data DealerTask
    = DCleanup
    | DShuffle
    | DAttendChips
    | DRespondToPlayer
    | DRespondToEnvironment
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data DealerTablePhase
    = DOffDuty
    | DPushing
    | DOnDuty
    | DTasking DealerTask
    | DLeaving
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data DealerTableFSM (p :: DealerTablePhase) where
    DOffDutyFSM :: DealerTableFSM 'DOffDuty
    DPushingFSM :: DealerTableFSM 'DPushing
    DOnDutyFSM :: DealerTableFSM 'DOnDuty
    DTaskingFSM :: DealerTask -> DealerTableFSM ('DTasking task)
    DLeavingFSM :: DealerTableFSM 'DLeaving

deriving instance Eq (DealerTableFSM p)
deriving instance Show (DealerTableFSM p)

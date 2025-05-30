module Pitboss.FSM.DealerTable.Phase where

import Data.Aeson.Types
import GHC.Generics

data DealerTask
    = DTCleanup
    | DTShuffle
    | DTAttendChips
    | DTRespondToPlayer
    | DTRespondToEnvironment
    deriving (Eq, Show, Generic)

data DealerTablePhase
    = DTOffDuty
    | DTPushing
    | DTOnDuty
    | DTTasking DealerTask
    | DTLeaving
    deriving (Eq, Show, Generic)

instance ToJSON DealerTask
instance FromJSON DealerTask
instance ToJSON DealerTablePhase
instance FromJSON DealerTablePhase

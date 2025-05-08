module Pitboss.FSM.DealerTableFSM.Phase where

import Data.Aeson.Types
import GHC.Generics

data DealerTask
  = Cleanup
  | Shuffle
  | AttendChips
  | RespondToPlayer
  | RespondToEnvironment
  deriving (Eq, Show, Generic)

data DealerTablePhase
  = OffDuty
  | Pushing
  | OnDuty
  | Tasking DealerTask
  | Leaving
  deriving (Eq, Show, Generic)

instance ToJSON DealerTask

instance FromJSON DealerTask

instance ToJSON DealerTablePhase

instance FromJSON DealerTablePhase

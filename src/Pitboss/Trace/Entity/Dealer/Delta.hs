module Pitboss.Trace.Entity.Dealer.Delta where

import Data.Aeson (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)
import Pitboss.FSM.DealerHand
import Pitboss.FSM.DealerRound
import Pitboss.FSM.DealerTable
import Pitboss.Trace.Entity.Types.EntityId

data DDealerAttrs
    = DDealerSetName String String
    deriving (Eq, Show, Generic)

instance ToJSON DDealerAttrs
instance FromJSON DDealerAttrs

data DDealerModes
    = DDealerSetTableFSM SomeDealerTableFSM SomeDealerTableFSM
    | DDealerSetRoundFSM DealerRoundFSM DealerRoundFSM
    | DDealerSetHandFSM SomeDealerHandFSM SomeDealerHandFSM
    deriving (Eq, Show, Generic)

instance ToJSON DDealerModes
instance FromJSON DDealerModes

data DDealerRels
    = DDealerSetActiveTable (Maybe (ClockedRef ETableId)) (Maybe (ClockedRef ETableId))
    | DDealerSetActiveRound (Maybe (ClockedRef EDealerRoundId)) (Maybe (ClockedRef EDealerRoundId))
    | DDealerSetActiveHand (Maybe (ClockedRef EDealerHandId)) (Maybe (ClockedRef EDealerHandId))
    deriving (Eq, Show, Generic)

instance ToJSON DDealerRels
instance FromJSON DDealerRels

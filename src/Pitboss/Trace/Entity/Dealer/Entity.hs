{-# LANGUAGE DataKinds #-}

module Pitboss.Trace.Entity.Dealer.Entity where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.FSM.DealerHand
import Pitboss.FSM.DealerRound
import Pitboss.FSM.DealerTable
import Pitboss.Trace.Entity.Types.EntityId

mkDealerEntityAttrs :: String -> Maybe (ClockedRef TableEntityId) -> DealerEntityAttrs
mkDealerEntityAttrs = DealerEntityAttrs

mkDealerEntityModes :: SomeDealerTableFSM -> DealerRoundFSM -> SomeDealerHandFSM -> DealerEntityModes
mkDealerEntityModes = DealerEntityModes

mkDealerEntityRels :: Maybe (ClockedRef DealerRoundEntityId) -> Maybe (ClockedRef DealerHandEntityId) -> DealerEntityRels
mkDealerEntityRels = DealerEntityRels

data DealerEntityAttrs = DealerEntityAttrs
    { _dealerEntityAttrsName :: String
    , _dealerEntityAttrsAssignedTable :: Maybe (ClockedRef TableEntityId)
    }
    deriving (Eq, Show, Generic)

data DealerEntityModes = DealerEntityModes
    { _dealerEntityModesDealerTable :: SomeDealerTableFSM
    , _dealerEntityModesDealerRound :: DealerRoundFSM
    , _dealerEntityModesDealerHand :: SomeDealerHandFSM
    }
    deriving (Eq, Show, Generic)

data DealerEntityRels = DealerEntityRels
    { _dealerEntityRelsCurrentRound :: Maybe (ClockedRef DealerRoundEntityId)
    , _dealerEntityRelsActiveHand :: Maybe (ClockedRef DealerHandEntityId)
    }
    deriving (Eq, Show, Generic)

instance ToJSON DealerEntityAttrs

instance FromJSON DealerEntityAttrs

instance ToJSON DealerEntityModes

instance FromJSON DealerEntityModes

instance ToJSON DealerEntityRels

instance FromJSON DealerEntityRels

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Pitboss.Trace.Entity.Dealer where

import Data.Aeson.Types
import GHC.Generics (Generic)
import Pitboss.FSM.DealerHand
import Pitboss.FSM.DealerRound
import Pitboss.FSM.DealerTable
import Pitboss.Trace.Entity.Types.Meta
import Pitboss.Trace.Types.EntityRef
import Pitboss.Trace.Types.Identifier

mkDealerEntity :: Meta (EntityRef DealerEntityId) -> DealerEntityAttrs -> DealerEntityModes -> DealerEntityRels -> DealerEntity
mkDealerEntity = DealerEntity

mkDealerEntityAttrs :: String -> Maybe (EntityRef TableEntityId) -> DealerEntityAttrs
mkDealerEntityAttrs = DealerEntityAttrs

mkDealerEntityModes :: SomeDealerTableFSM -> DealerRoundFSM -> SomeDealerHandFSM -> DealerEntityModes
mkDealerEntityModes = DealerEntityModes

mkDealerEntityRels :: Maybe (EntityRef DealerRoundEntityId) -> Maybe (EntityRef DealerHandEntityId) -> DealerEntityRels
mkDealerEntityRels = DealerEntityRels

data DealerEntity = DealerEntity
    { _dealerEntityMeta :: Meta (EntityRef DealerEntityId)
    , _dealerEntityAttrs :: DealerEntityAttrs
    , _dealerEntityModes :: DealerEntityModes
    , _dealerEntityRels :: DealerEntityRels
    }
    deriving (Eq, Show, Generic)

data DealerEntityAttrs = DealerEntityAttrs
    { _dealerEntityAttrsName :: String
    , _dealerEntityAttrsAssignedTable :: Maybe (EntityRef TableEntityId)
    }
    deriving (Eq, Show, Generic)

data DealerEntityModes = DealerEntityModes
    { _dealerEntityModesDealerTable :: SomeDealerTableFSM
    , _dealerEntityModesDealerRound :: DealerRoundFSM
    , _dealerEntityModesDealerHand :: SomeDealerHandFSM
    }
    deriving (Eq, Show, Generic)

data DealerEntityRels = DealerEntityRels
    { _dealerEntityRelsCurrentRound :: Maybe (EntityRef DealerRoundEntityId)
    , _dealerEntityRelsActiveHand :: Maybe (EntityRef DealerHandEntityId)
    }
    deriving (Eq, Show, Generic)

instance ToJSON DealerEntity

instance FromJSON DealerEntity

instance ToJSON DealerEntityAttrs

instance FromJSON DealerEntityAttrs

instance ToJSON DealerEntityModes

instance FromJSON DealerEntityModes

instance ToJSON DealerEntityRels

instance FromJSON DealerEntityRels

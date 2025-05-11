{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Pitboss.Trace.Entity.DealerRound where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Trace.Entity.Types.Meta
import Pitboss.Trace.Types.EntityRef
import Pitboss.Trace.Types.Identifier

mkDealerRoundEntityAttrs :: Int -> DealerRoundEntityAttrs
mkDealerRoundEntityAttrs n = DealerRoundEntityAttrs n False

mkDealerRoundEntity :: Meta DealerRoundEntityId -> DealerRoundEntityAttrs -> DealerRoundEntityModes -> DealerRoundEntityRels -> DealerRoundEntity
mkDealerRoundEntity = DealerRoundEntity

mkDealerRoundEntityModes :: DealerRoundEntityModes
mkDealerRoundEntityModes = DealerRoundEntityModes

mkDealerRoundEntityRels :: EntityRef TableShoeEntityId -> DealerRoundEntityRels
mkDealerRoundEntityRels = DealerRoundEntityRels

data DealerRoundEntity = DealerRoundEntity
    { _dealerRoundEntityMeta :: Meta DealerRoundEntityId
    , _dealerRoundEntityAttrs :: DealerRoundEntityAttrs
    , _dealerRoundEntityModes :: DealerRoundEntityModes
    , _dealerRoundEntityRels :: DealerRoundEntityRels
    }
    deriving (Eq, Show, Generic)

data DealerRoundEntityAttrs = DealerRoundEntityAttrs
    { _dealerRoundEntityAttrsNumber :: Int
    , _dealerRoundEntityAttrsIsActive :: Bool
    }
    deriving (Eq, Show, Generic)

data DealerRoundEntityModes = DealerRoundEntityModes
    {
    }
    deriving (Eq, Show, Generic)

data DealerRoundEntityRels = DealerRoundEntityRels
    { _dealerRoundEntityRelsTableShoeUsed :: EntityRef TableShoeEntityId
    }
    deriving (Eq, Show, Generic)

instance ToJSON DealerRoundEntity

instance FromJSON DealerRoundEntity

instance ToJSON DealerRoundEntityAttrs

instance FromJSON DealerRoundEntityAttrs

instance ToJSON DealerRoundEntityModes

instance FromJSON DealerRoundEntityModes

instance ToJSON DealerRoundEntityRels

instance FromJSON DealerRoundEntityRels

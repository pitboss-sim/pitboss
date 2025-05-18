{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Pitboss.Trace.Entity.DealerRound where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Trace.Entity.DealerRound.Types
import Pitboss.Trace.Entity.Types.Meta
import Pitboss.Trace.Types.Identifier

mkDealerRoundEntity :: Meta DealerRoundEntityId -> DealerRoundEntityAttrs -> DealerRoundEntityModes -> DealerRoundEntityRels -> DealerRoundEntity
mkDealerRoundEntity = DealerRoundEntity

data DealerRoundEntity = DealerRoundEntity
    { _dealerRoundEntityMeta :: Meta DealerRoundEntityId
    , _dealerRoundEntityAttrs :: DealerRoundEntityAttrs
    , _dealerRoundEntityModes :: DealerRoundEntityModes
    , _dealerRoundEntityRels :: DealerRoundEntityRels
    }
    deriving (Eq, Show, Generic)

instance ToJSON DealerRoundEntity

instance FromJSON DealerRoundEntity

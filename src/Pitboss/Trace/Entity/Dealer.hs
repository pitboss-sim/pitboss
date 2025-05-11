{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Pitboss.Trace.Entity.Dealer where

import Data.Aeson.Types
import GHC.Generics (Generic)
import Pitboss.Trace.Entity.Dealer.Types
import Pitboss.Trace.Entity.Types.Meta
import Pitboss.Trace.Types.EntityRef
import Pitboss.Trace.Types.Identifier

mkDealerEntity :: Meta (EntityRef DealerEntityId) -> DealerEntityAttrs -> DealerEntityModes -> DealerEntityRels -> DealerEntity
mkDealerEntity = DealerEntity

data DealerEntity = DealerEntity
    { _dealerEntityMeta :: Meta (EntityRef DealerEntityId)
    , _dealerEntityAttrs :: DealerEntityAttrs
    , _dealerEntityModes :: DealerEntityModes
    , _dealerEntityRels :: DealerEntityRels
    }
    deriving (Eq, Show, Generic)

instance ToJSON DealerEntity

instance FromJSON DealerEntity

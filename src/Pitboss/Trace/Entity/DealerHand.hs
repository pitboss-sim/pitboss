{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Pitboss.Trace.Entity.DealerHand where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Trace.Entity.DealerHand.Types
import Pitboss.Trace.Entity.Types.Meta
import Pitboss.Trace.Types.EntityRef
import Pitboss.Trace.Types.Identifier

mkDealerHandEntity :: Meta (EntityRef DealerHandEntityId) -> DealerHandEntityAttrs -> DealerHandEntityModes -> DealerHandEntityRels -> DealerHandEntity
mkDealerHandEntity = DealerHandEntity

data DealerHandEntity = DealerHandEntity
    { _dealerHandEntityMeta :: Meta (EntityRef DealerHandEntityId)
    , _dealerHandEntityAttrs :: DealerHandEntityAttrs
    , _dealerHandEntityModes :: DealerHandEntityModes
    , _dealerHandEntityRels :: DealerHandEntityRels
    }
    deriving (Eq, Show, Generic)

instance ToJSON DealerHandEntity

instance FromJSON DealerHandEntity

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Pitboss.Trace.Entity.DealerRound.Entity where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Trace.Entity.Types.EntityId

mkDealerRoundEntityAttrs :: Int -> Bool -> DealerRoundEntityAttrs
mkDealerRoundEntityAttrs = DealerRoundEntityAttrs

mkDealerRoundEntityModes :: DealerRoundEntityModes
mkDealerRoundEntityModes = DealerRoundEntityModes

mkDealerRoundEntityRels :: ClockedRef TableShoeEntityId -> DealerRoundEntityRels
mkDealerRoundEntityRels = DealerRoundEntityRels

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
    { _dealerRoundEntityRelsTableShoeUsed :: ClockedRef TableShoeEntityId
    }
    deriving (Eq, Show, Generic)

instance ToJSON DealerRoundEntityAttrs

instance FromJSON DealerRoundEntityAttrs

instance ToJSON DealerRoundEntityModes

instance FromJSON DealerRoundEntityModes

instance ToJSON DealerRoundEntityRels

instance FromJSON DealerRoundEntityRels

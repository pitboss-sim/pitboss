{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Pitboss.Trace.Entity.DealerRound.Types where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Trace.Types.EntityRef
import Pitboss.Trace.Types.Identifier

mkDealerRoundEntityAttrs :: Int -> Bool -> DealerRoundEntityAttrs
mkDealerRoundEntityAttrs = DealerRoundEntityAttrs

mkDealerRoundEntityModes :: DealerRoundEntityModes
mkDealerRoundEntityModes = DealerRoundEntityModes

mkDealerRoundEntityRels :: EntityRef TableShoeEntityId -> DealerRoundEntityRels
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
    { _dealerRoundEntityRelsTableShoeUsed :: EntityRef TableShoeEntityId
    }
    deriving (Eq, Show, Generic)

instance ToJSON DealerRoundEntityAttrs

instance FromJSON DealerRoundEntityAttrs

instance ToJSON DealerRoundEntityModes

instance FromJSON DealerRoundEntityModes

instance ToJSON DealerRoundEntityRels

instance FromJSON DealerRoundEntityRels

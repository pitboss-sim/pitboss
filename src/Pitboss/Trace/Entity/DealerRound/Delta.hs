{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Pitboss.Trace.Entity.DealerRound.Delta where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Trace.Entity.Types.EntityId

data DealerRoundEntityAttrsDelta
    = SetDealerRoundEntityNumber Int
    | SetActive Bool
    deriving (Eq, Show, Generic)

instance ToJSON DealerRoundEntityAttrsDelta
instance FromJSON DealerRoundEntityAttrsDelta

data DealerRoundEntityModesDelta = NoopModes
    deriving (Eq, Show, Generic)

instance ToJSON DealerRoundEntityModesDelta
instance FromJSON DealerRoundEntityModesDelta

data DealerRoundEntityRelsDelta
    = SetTableShoeUsed (ClockedRef TableShoeEntityId)
    deriving (Eq, Show, Generic)

instance ToJSON DealerRoundEntityRelsDelta
instance FromJSON DealerRoundEntityRelsDelta

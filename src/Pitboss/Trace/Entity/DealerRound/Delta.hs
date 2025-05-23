{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Pitboss.Trace.Entity.DealerRound.Delta where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Trace.Entity.Types.EntityId

data DDealerRoundAttrs
    = DDealerRoundSetNumber Int Int
    deriving (Eq, Show, Generic)

instance ToJSON DDealerRoundAttrs
instance FromJSON DDealerRoundAttrs

data DDealerRoundModes = DDealerRoundModes
    deriving (Eq, Show, Generic)

instance ToJSON DDealerRoundModes
instance FromJSON DDealerRoundModes

data DDealerRoundRels
    = DDealerRoundSetTableShoe (ClockedRef ETableShoeId) (ClockedRef ETableShoeId)
    deriving (Eq, Show, Generic)

instance ToJSON DDealerRoundRels
instance FromJSON DDealerRoundRels

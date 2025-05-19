{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Pitboss.Trace.Entity.DealerRound.Entity where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Trace.Entity.Types.EntityId

mkEDealerRoundAttrs :: Int -> Bool -> EDealerRoundAttrs
mkEDealerRoundAttrs = EDealerRoundAttrs

mkEDealerRoundModes :: EDealerRoundModes
mkEDealerRoundModes = EDealerRoundModes

mkEDealerRoundRels :: ClockedRef ETableShoeId -> EDealerRoundRels
mkEDealerRoundRels = EDealerRoundRels

data EDealerRoundAttrs = EDealerRoundAttrs
    { _drAttrsNumber :: Int
    , _drAttrsIsActive :: Bool
    }
    deriving (Eq, Show, Generic)

data EDealerRoundModes = EDealerRoundModes
    {
    }
    deriving (Eq, Show, Generic)

data EDealerRoundRels = EDealerRoundRels
    { _drRelsTableShoeUsed :: ClockedRef ETableShoeId
    }
    deriving (Eq, Show, Generic)

instance ToJSON EDealerRoundAttrs

instance FromJSON EDealerRoundAttrs

instance ToJSON EDealerRoundModes

instance FromJSON EDealerRoundModes

instance ToJSON EDealerRoundRels

instance FromJSON EDealerRoundRels

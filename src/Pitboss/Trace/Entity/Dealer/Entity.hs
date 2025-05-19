{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Pitboss.Trace.Entity.Dealer.Entity where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.FSM.DealerHand
import Pitboss.FSM.DealerRound
import Pitboss.FSM.DealerTable
import Pitboss.Trace.Entity.Types.EntityId

mkEDealerAttrs :: String -> EDealerAttrs
mkEDealerAttrs = EDealerAttrs

mkEDealerModes :: SomeDealerTableFSM -> DealerRoundFSM -> SomeDealerHandFSM -> EDealerModes
mkEDealerModes = EDealerModes

mkEDealerRels :: Maybe (ClockedRef ETableId) -> Maybe (ClockedRef EDealerRoundId) -> Maybe (ClockedRef EDealerHandId) -> EDealerRels
mkEDealerRels = EDealerRels

data EDealerAttrs = EDealerAttrs
    { _dAttrsName :: String
    }
    deriving (Eq, Show, Generic)

data EDealerModes = EDealerModes
    { _dModesDealerTable :: SomeDealerTableFSM
    , _dModesDealerRound :: DealerRoundFSM
    , _dModesDealerHand :: SomeDealerHandFSM
    }
    deriving (Eq, Show, Generic)

data EDealerRels = EDealerRels
    { _dRelsActiveTable :: Maybe (ClockedRef ETableId)
    , _dRelsActiveRound :: Maybe (ClockedRef EDealerRoundId)
    , _dRelsActiveHand :: Maybe (ClockedRef EDealerHandId)
    }
    deriving (Eq, Show, Generic)

instance ToJSON EDealerAttrs
instance FromJSON EDealerAttrs

instance ToJSON EDealerModes
instance FromJSON EDealerModes

instance ToJSON EDealerRels
instance FromJSON EDealerRels

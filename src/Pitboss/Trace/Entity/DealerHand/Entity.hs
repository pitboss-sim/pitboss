{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Pitboss.Trace.Entity.DealerHand.Entity where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Card
import Pitboss.FSM.DealerHand
import Pitboss.Trace.Entity.Types.EntityId

mkEDealerHandAttrs :: [Card] -> EDealerHandAttrs
mkEDealerHandAttrs = EDealerHandAttrs

mkEDealerHandModes :: SomeDealerHandFSM -> EDealerHandModes
mkEDealerHandModes = EDealerHandModes

mkEDealerHandRels :: ClockedRef EDealerRoundId -> ClockedRef EDealerId -> EDealerHandRels
mkEDealerHandRels = EDealerHandRels

data EDealerHandAttrs = EDealerHandAttrs
    { _dhAttrsHandCards :: [Card]
    }
    deriving (Eq, Show, Generic)

data EDealerHandModes = EDealerHandModes
    { _dhModesDealerHand :: SomeDealerHandFSM
    }
    deriving (Eq, Show, Generic)

data EDealerHandRels = EDealerHandRels
    { _dhRelsDealerRound :: ClockedRef EDealerRoundId
    , _dhRelsDealer :: ClockedRef EDealerId
    }
    deriving (Eq, Show, Generic)

instance ToJSON EDealerHandAttrs
instance FromJSON EDealerHandAttrs

instance ToJSON EDealerHandModes
instance FromJSON EDealerHandModes

instance ToJSON EDealerHandRels
instance FromJSON EDealerHandRels

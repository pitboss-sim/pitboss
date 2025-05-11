{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Pitboss.Trace.Entity.DealerHand where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Card (Card)
import Pitboss.FSM.DealerHand
import Pitboss.Trace.Entity.Types.Meta
import Pitboss.Trace.Types.EntityRef
import Pitboss.Trace.Types.Identifier

mkDealerHandEntity :: Meta (EntityRef DealerHandEntityId) -> DealerHandEntityAttrs -> DealerHandEntityModes -> DealerHandEntityRels -> DealerHandEntity
mkDealerHandEntity = DealerHandEntity

mkDealerHandEntityAttrs :: [Card] -> DealerHandEntityAttrs
mkDealerHandEntityAttrs = DealerHandEntityAttrs

mkDealerHandEntityModes :: SomeDealerHandFSM -> DealerHandEntityModes
mkDealerHandEntityModes = DealerHandEntityModes

mkDealerHandEntityRels :: EntityRef PlayerSpotEntityId -> EntityRef DealerRoundEntityId -> EntityRef DealerEntityId -> DealerHandEntityRels
mkDealerHandEntityRels = DealerHandEntityRels

data DealerHandEntity = DealerHandEntity
    { _dealerHandEntityMeta :: Meta (EntityRef DealerHandEntityId)
    , _dealerHandEntityAttrs :: DealerHandEntityAttrs
    , _dealerHandEntityModes :: DealerHandEntityModes
    , _dealerHandEntityRels :: DealerHandEntityRels
    }
    deriving (Eq, Show, Generic)

data DealerHandEntityAttrs = DealerHandEntityAttrs
    { _dealerHandEntityAttrsHandCards :: [Card]
    }
    deriving (Eq, Show, Generic)

data DealerHandEntityModes = DealerHandEntityModes
    { _dealerHandEntityModesDealerHand :: SomeDealerHandFSM
    }
    deriving (Eq, Show, Generic)

data DealerHandEntityRels = DealerHandEntityRels
    { _dealerHandEntityRelsBelongsToPlayerSpot :: EntityRef PlayerSpotEntityId
    , _dealerHandEntityRelsBelongsToDealerRound :: EntityRef DealerRoundEntityId
    , _dealerHandEntityRelsOwnedByDealer :: EntityRef DealerEntityId
    }
    deriving (Eq, Show, Generic)

instance ToJSON DealerHandEntity

instance FromJSON DealerHandEntity

instance ToJSON DealerHandEntityAttrs

instance FromJSON DealerHandEntityAttrs

instance ToJSON DealerHandEntityModes

instance FromJSON DealerHandEntityModes

instance ToJSON DealerHandEntityRels

instance FromJSON DealerHandEntityRels

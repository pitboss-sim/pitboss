{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Pitboss.Trace.Entity.PlayerSpot.Types where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Chips
import Pitboss.FSM.PlayerSpot
import Pitboss.Trace.Entity.Types
import Pitboss.Trace.Entity.Types.FiniteMap.BoundedEnum
import Pitboss.Trace.Entity.Types.FiniteMap.Occupancy
import Pitboss.Trace.Types.EntityRef
import Pitboss.Trace.Types.Identifier

mkPlayerSpotEntityAttrs :: PlayerSpotIx -> Chips -> PlayerSpotEntityAttrs
mkPlayerSpotEntityAttrs = PlayerSpotEntityAttrs

mkPlayerSpotEntityModes :: SomePlayerSpotFSM -> PlayerSpotEntityModes
mkPlayerSpotEntityModes = PlayerSpotEntityModes

mkPlayerSpotEntityRels :: EntityRef PlayerEntityId -> EntityRef DealerRoundEntityId -> FiniteMap PlayerSpotHandIx (Occupancy (EntityRef PlayerHandEntityId)) -> PlayerSpotEntityRels
mkPlayerSpotEntityRels = PlayerSpotEntityRels

data PlayerSpotIx = PlayerSpotEntity1 | PlayerSpotEntity2 | PlayerSpotEntity3 | PlayerSpotEntity4
    deriving (Eq, Show, Ord, Enum, Bounded, Generic)

instance ToJSONKey PlayerSpotIx

instance FromJSONKey PlayerSpotIx

instance ToJSON PlayerSpotIx

instance FromJSON PlayerSpotIx

instance BoundedEnum PlayerSpotIx

data PlayerSpotEntityAttrs = PlayerSpotEntityAttrs
    { _playerSpotEntityAttrsSpotIndex :: PlayerSpotIx
    , _playerSpotEntityAttrsWager :: Chips
    }
    deriving (Eq, Show, Generic)

data PlayerSpotEntityModes = PlayerSpotEntityModes
    { _playerSpotEntityModesPlayerSpot :: SomePlayerSpotFSM
    }
    deriving (Eq, Show, Generic)

data PlayerSpotHandIx = PlayerSpotEntityHand1 | PlayerSpotEntityHand2 | PlayerSpotEntityHand3 | PlayerSpotEntityHand4
    deriving (Eq, Show, Ord, Enum, Bounded, Generic)

instance ToJSONKey PlayerSpotHandIx

instance FromJSONKey PlayerSpotHandIx

instance ToJSON PlayerSpotHandIx

instance FromJSON PlayerSpotHandIx

instance BoundedEnum PlayerSpotHandIx

data PlayerSpotEntityRels = PlayerSpotEntityRels
    { _playerSpotEntityRelsPlayerEntityId :: EntityRef PlayerEntityId
    , _playerSpotEntityRelsRoundEntityId :: EntityRef DealerRoundEntityId
    , _playerSpotEntityRelsHandOccupancy :: FiniteMap PlayerSpotHandIx (Occupancy (EntityRef PlayerHandEntityId))
    }
    deriving (Eq, Show, Generic)

instance ToJSON PlayerSpotEntityAttrs

instance FromJSON PlayerSpotEntityAttrs

instance ToJSON PlayerSpotEntityModes

instance FromJSON PlayerSpotEntityModes

instance ToJSON PlayerSpotEntityRels

instance FromJSON PlayerSpotEntityRels

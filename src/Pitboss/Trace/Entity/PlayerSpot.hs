{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Pitboss.Trace.Entity.PlayerSpot where

import Data.Aeson
import GHC.Generics (Generic)
import Pitboss.Blackjack.Chips (Chips)
import Pitboss.FSM.PlayerSpot
import Pitboss.Trace.Entity.Types.FiniteMap
import Pitboss.Trace.Entity.Types.FiniteMap.BoundedEnum
import Pitboss.Trace.Entity.Types.FiniteMap.Occupancy
import Pitboss.Trace.Entity.Types.Meta
import Pitboss.Trace.Types.EntityRef
import Pitboss.Trace.Types.Identifier

mkPlayerSpotEntity :: Meta (EntityRef PlayerSpotEntityId) -> PlayerSpotEntityAttrs -> PlayerSpotEntityModes -> PlayerSpotEntityRels -> PlayerSpotEntity
mkPlayerSpotEntity = PlayerSpotEntity

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

data PlayerSpotHandIx = PlayerSpotEntityHand1 | PlayerSpotEntityHand2 | PlayerSpotEntityHand3 | PlayerSpotEntityHand4
  deriving (Eq, Show, Ord, Enum, Bounded, Generic)

instance ToJSONKey PlayerSpotHandIx

instance FromJSONKey PlayerSpotHandIx

instance ToJSON PlayerSpotHandIx

instance FromJSON PlayerSpotHandIx

instance BoundedEnum PlayerSpotHandIx

data PlayerSpotEntity = PlayerSpotEntity
  { _playerSpotEntityMeta :: Meta (EntityRef PlayerSpotEntityId),
    _playerSpotEntityAttrs :: PlayerSpotEntityAttrs,
    _playerSpotEntityModes :: PlayerSpotEntityModes,
    _playerSpotEntityRels :: PlayerSpotEntityRels
  }
  deriving (Eq, Show, Generic)

data PlayerSpotEntityAttrs = PlayerSpotEntityAttrs
  { _playerSpotEntityAttrsSpotIndex :: PlayerSpotIx,
    _playerSpotEntityAttrsWager :: Chips
  }
  deriving (Eq, Show, Generic)

data PlayerSpotEntityModes = PlayerSpotEntityModes
  { _playerSpotEntityModesPlayerSpot :: SomePlayerSpotFSM
  }
  deriving (Eq, Show, Generic)

data PlayerSpotEntityRels = PlayerSpotEntityRels
  { _playerSpotEntityRelsPlayerEntityId :: EntityRef PlayerEntityId,
    _playerSpotEntityRelsRoundEntityId :: EntityRef DealerRoundEntityId,
    _playerSpotEntityRelsHandOccupancy :: FiniteMap PlayerSpotHandIx (Occupancy (EntityRef PlayerHandEntityId))
  }
  deriving (Eq, Show, Generic)

instance ToJSON PlayerSpotEntity

instance FromJSON PlayerSpotEntity

instance ToJSON PlayerSpotEntityAttrs

instance FromJSON PlayerSpotEntityAttrs

instance ToJSON PlayerSpotEntityModes

instance FromJSON PlayerSpotEntityModes

instance ToJSON PlayerSpotEntityRels

instance FromJSON PlayerSpotEntityRels

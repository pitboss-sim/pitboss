{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

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

mkPlayerSpot :: Meta (EntityRef PlayerSpotId) -> SomePlayerSpotFSM -> PlayerSpotState -> PlayerSpotRelations -> PlayerSpot
mkPlayerSpot = PlayerSpot

mkPlayerSpotState :: PlayerSpotIx -> Chips -> PlayerSpotState
mkPlayerSpotState = PlayerSpotState

mkPlayerSpotRelations :: EntityRef PlayerId -> EntityRef DealerRoundId -> FiniteMap PlayerSpotHandIx (Occupancy (EntityRef PlayerHandId)) -> PlayerSpotRelations
mkPlayerSpotRelations = PlayerSpotRelations

data PlayerSpotIx = PlayerSpot1 | PlayerSpot2 | PlayerSpot3 | PlayerSpot4
  deriving (Eq, Show, Ord, Enum, Bounded, Generic)

instance ToJSONKey PlayerSpotIx

instance FromJSONKey PlayerSpotIx

instance ToJSON PlayerSpotIx

instance FromJSON PlayerSpotIx

instance BoundedEnum PlayerSpotIx

data PlayerSpotHandIx = PlayerSpotHand1 | PlayerSpotHand2 | PlayerSpotHand3 | PlayerSpotHand4
  deriving (Eq, Show, Ord, Enum, Bounded, Generic)

instance ToJSONKey PlayerSpotHandIx

instance FromJSONKey PlayerSpotHandIx

instance ToJSON PlayerSpotHandIx

instance FromJSON PlayerSpotHandIx

instance BoundedEnum PlayerSpotHandIx

data PlayerSpot = PlayerSpot
  { _playerSpotMeta :: Meta (EntityRef PlayerSpotId),
    _playerSpotFsm :: SomePlayerSpotFSM,
    _playerSpotState :: PlayerSpotState,
    _playerSpotRels :: PlayerSpotRelations
  }
  deriving (Eq, Show, Generic)

data PlayerSpotState = PlayerSpotState
  { _playerSpotStateSpotIndex :: PlayerSpotIx,
    _playerSpotStateWager :: Chips
  }
  deriving (Eq, Show, Generic)

data PlayerSpotRelations = PlayerSpotRelations
  { _playerSpotRelsPlayerId :: EntityRef PlayerId,
    _playerSpotRelsRoundId :: EntityRef DealerRoundId,
    _playerSpotRelsHandOccupancy :: FiniteMap PlayerSpotHandIx (Occupancy (EntityRef PlayerHandId))
  }
  deriving (Eq, Show, Generic)

instance ToJSON PlayerSpot

instance FromJSON PlayerSpot

instance ToJSON PlayerSpotState

instance FromJSON PlayerSpotState

instance ToJSON PlayerSpotRelations

instance FromJSON PlayerSpotRelations

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.PlayerSpot.Delta where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Chips
import Pitboss.FSM.PlayerSpot
import Pitboss.Trace.Entity.PlayerSpot.Entity
import Pitboss.Trace.Entity.Types.EntityId
import Pitboss.Trace.Entity.Types.FiniteMap.Occupancy

data DPlayerSpotAttrs
    = DPlayerSpotSetWager Chips Chips
    deriving (Eq, Show, Generic)

instance ToJSON DPlayerSpotAttrs
instance FromJSON DPlayerSpotAttrs

data DPlayerSpotModes
    = DPlayerSpotSetFSM SomePlayerSpotFSM SomePlayerSpotFSM
    deriving (Eq, Show, Generic)

instance ToJSON DPlayerSpotModes
instance FromJSON DPlayerSpotModes

data DPlayerSpotRels
    = DPlayerSpotSetPlayer (ClockedRef EPlayerId) (ClockedRef EPlayerId)
    | DPlayerSpotSetRound (ClockedRef EDealerRoundId) (ClockedRef EDealerRoundId)
    | DPlayerSpotSetHandOccupancy
        (PlayerSpotHandIx, Occupancy (ClockedRef EPlayerHandId))
        (PlayerSpotHandIx, Occupancy (ClockedRef EPlayerHandId))
    deriving (Eq, Show, Generic)

instance ToJSON DPlayerSpotRels
instance FromJSON DPlayerSpotRels

module Pitboss.Trace.Entity.PlayerSpot.Delta where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Chips
import Pitboss.FSM.PlayerSpot
import Pitboss.Trace.Entity.PlayerSpot.Types
import Pitboss.Trace.Entity.Types
import Pitboss.Trace.Entity.Types.EntityId
import Pitboss.Trace.Entity.Types.FiniteMap.Occupancy

data PlayerSpotEntityAttrsDelta
    = ReplaceWager Chips Chips
    deriving (Eq, Show, Generic)

instance ToJSON PlayerSpotEntityAttrsDelta
instance FromJSON PlayerSpotEntityAttrsDelta

data PlayerSpotEntityModesDelta
    = ReplaceFSM SomePlayerSpotFSM SomePlayerSpotFSM
    deriving (Eq, Show, Generic)

instance ToJSON PlayerSpotEntityModesDelta
instance FromJSON PlayerSpotEntityModesDelta

data PlayerSpotEntityRelsDelta
    = UpdatePlayer (ClockedRef PlayerEntityId) (ClockedRef PlayerEntityId)
    | UpdateRound (ClockedRef DealerRoundEntityId) (ClockedRef DealerRoundEntityId)
    | UpdateHandOccupancy
        (PlayerSpotHandIx, Occupancy (ClockedRef PlayerHandEntityId))
        (PlayerSpotHandIx, Occupancy (ClockedRef PlayerHandEntityId))
    deriving (Eq, Show, Generic)

instance ToJSON PlayerSpotEntityRelsDelta
instance FromJSON PlayerSpotEntityRelsDelta


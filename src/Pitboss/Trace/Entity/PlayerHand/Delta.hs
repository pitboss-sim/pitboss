module Pitboss.Trace.Entity.PlayerHand.Delta where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Card
import Pitboss.FSM.PlayerHand
import Pitboss.Trace.Entity.Types.EntityId

data DPlayerHandAttrs
    = DPlayerHandSetPlayerHandIx Int Int
    | DPlayerHandSetSplitDepth Int Int
    | DPlayerHandPushCard Card [Card]
    | DPlayerHandPopCard Card [Card]
    | DPlayerHandSetCards [Card] [Card]
    deriving (Eq, Show, Generic)

instance ToJSON DPlayerHandAttrs
instance FromJSON DPlayerHandAttrs

data DPlayerHandModes
    = DPlayerHandSetPlayerHandFSM SomePlayerHandFSM SomePlayerHandFSM
    deriving (Eq, Show, Generic)

instance ToJSON DPlayerHandModes
instance FromJSON DPlayerHandModes

data DPlayerHandRels
    = DPlayerHandSetPlayerSpot (ClockedRef EPlayerSpotId) (ClockedRef EPlayerSpotId)
    deriving (Eq, Show, Generic)

instance ToJSON DPlayerHandRels
instance FromJSON DPlayerHandRels

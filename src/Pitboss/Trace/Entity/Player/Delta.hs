module Pitboss.Trace.Entity.Player.Delta where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Chips
import Pitboss.Trace.Entity.Types.EntityId

data DPlayerAttrs
    = DPlayerSetName String String
    | DPlayerSetBankroll Chips Chips
    deriving (Eq, Show, Generic)

instance ToJSON DPlayerAttrs
instance FromJSON DPlayerAttrs

data DPlayerModes = DPlayerModes
    deriving (Eq, Show, Generic)

instance ToJSON DPlayerModes
instance FromJSON DPlayerModes

data DPlayerRels
    = DPlayerSetTable (Maybe (ClockedRef ETableId)) (Maybe (ClockedRef ETableId))
    | DPlayerSetSpot (Maybe (ClockedRef EPlayerSpotId)) (Maybe (ClockedRef EPlayerSpotId))
    | DPlayerSetHand (Maybe (ClockedRef EPlayerHandId)) (Maybe (ClockedRef EPlayerHandId))
    deriving (Eq, Show, Generic)

instance ToJSON DPlayerRels
instance FromJSON DPlayerRels

module Pitboss.Trace.Entity.Table.Delta where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Chips
import Pitboss.Trace.Entity.Types
import Pitboss.Trace.Entity.Types.EntityId

data TableEntityAttrsDelta
    = SetTableName String String
    | SetMinBet Chips Chips
    | SetOffering (ClockedRef OfferingEntityId) (ClockedRef OfferingEntityId)
    | StartRound (Maybe (ClockedRef DealerRoundEntityId)) (ClockedRef DealerRoundEntityId)
    | EndRound (ClockedRef DealerRoundEntityId)
    deriving (Eq, Show, Generic)

instance ToJSON TableEntityAttrsDelta
instance FromJSON TableEntityAttrsDelta

data TableEntityModesDelta = NoopModes
    deriving (Eq, Show, Generic)

instance ToJSON TableEntityModesDelta
instance FromJSON TableEntityModesDelta

data TableEntityRelsDelta
    = AssignDealer (Maybe (ClockedRef DealerEntityId)) (ClockedRef DealerEntityId)
    | UnassignDealer (ClockedRef DealerEntityId)
    deriving (Eq, Show, Generic)

instance ToJSON TableEntityRelsDelta
instance FromJSON TableEntityRelsDelta

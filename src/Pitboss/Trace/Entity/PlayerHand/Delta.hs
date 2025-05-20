module Pitboss.Trace.Entity.PlayerHand.Delta where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Card
import Pitboss.FSM.PlayerHand
import Pitboss.Trace.Entity.Types
import Pitboss.Trace.Entity.Types.EntityId

data PlayerHandEntityAttrsDelta
    = AddCard Card
    | RemoveCard Card
    | ReplaceCards [Card] [Card]
    | ReplacePlayerHandIndex Int Int
    | ReplaceSplitDepth Int Int
    deriving (Eq, Show, Generic)

instance ToJSON PlayerHandEntityAttrsDelta
instance FromJSON PlayerHandEntityAttrsDelta

data PlayerHandEntityModesDelta
    = ReplaceFSM SomePlayerHandFSM SomePlayerHandFSM
    deriving (Eq, Show, Generic)

instance ToJSON PlayerHandEntityModesDelta
instance FromJSON PlayerHandEntityModesDelta

data PlayerHandEntityRelsDelta
    = UpdatePlayerSpot (ClockedRef PlayerSpotEntityId) (ClockedRef PlayerSpotEntityId)
    | UpdateDealerRound (ClockedRef DealerRoundEntityId) (ClockedRef DealerRoundEntityId)
    | UpdatePlayer (ClockedRef PlayerEntityId) (ClockedRef PlayerEntityId)
    deriving (Eq, Show, Generic)

instance ToJSON PlayerHandEntityRelsDelta
instance FromJSON PlayerHandEntityRelsDelta

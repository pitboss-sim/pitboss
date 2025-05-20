module Pitboss.Trace.Entity.Player.Delta where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Chips
import Pitboss.Trace.Entity.Types
import Pitboss.Trace.Entity.Types.EntityId

data PlayerEntityAttrsDelta
    = RenamePlayer String String
    | SetBankroll Chips Chips
    deriving (Eq, Show, Generic)

instance ToJSON PlayerEntityAttrsDelta
instance FromJSON PlayerEntityAttrsDelta

data PlayerEntityModesDelta = NoopModes
    deriving (Eq, Show, Generic)

instance ToJSON PlayerEntityModesDelta
instance FromJSON PlayerEntityModesDelta

data PlayerEntityRelsDelta
    = UpdateCloneOf (Maybe (ClockedRef PlayerEntityId)) (Maybe (ClockedRef PlayerEntityId))
    | UpdateSeatedAt (Maybe (ClockedRef TableEntityId)) (Maybe (ClockedRef TableEntityId))
    deriving (Eq, Show, Generic)

instance ToJSON PlayerEntityRelsDelta
instance FromJSON PlayerEntityRelsDelta

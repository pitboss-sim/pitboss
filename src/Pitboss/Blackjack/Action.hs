module Pitboss.Blackjack.Action where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data Move
    = Hit
    | Stand
    | Double
    | Split
    | Surrender
    deriving (Eq, Show, Generic)

instance ToJSON Move
instance FromJSON Move

data PlayerAction
    = PlayerHit
    | PlayerStand
    | PlayerDouble
    | PlayerSplit
    | PlayerSurrender
    | TakeInsurance
    | DeclineInsurance
    deriving (Eq, Show, Ord, Enum)

data DealerAction
    = DealerHit
    | DealerStand
    deriving (Eq, Show, Ord, Enum)

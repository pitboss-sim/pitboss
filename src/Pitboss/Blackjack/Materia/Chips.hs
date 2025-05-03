module Pitboss.Blackjack.Materia.Chips where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

newtype Chips = Chips Int
    deriving (Eq, Ord, Show, Num, Generic)

instance ToJSON Chips
instance FromJSON Chips

module Pitboss.FSM.PlayerTable.Phase where

import Data.Aeson.Types
import GHC.Generics

data PlayerPhase
    = Idle
    | ChoosingTable
    | PlacingBet
    | PlayingHand
    | Observing
    | Done
    deriving (Eq, Show, Generic)

instance ToJSON PlayerPhase

instance FromJSON PlayerPhase

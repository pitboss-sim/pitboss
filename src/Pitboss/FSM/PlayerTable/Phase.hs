module Pitboss.FSM.PlayerTable.Phase where

import Data.Aeson.Types
import GHC.Generics

data PlayerTablePhase
    = PTIdle
    | PTChoosingTable
    | PTPlacingBet
    | PTPlayingHand
    | PTObserving
    | PTDone
    deriving (Eq, Show, Generic)

instance ToJSON PlayerTablePhase
instance FromJSON PlayerTablePhase

{-# LANGUAGE DataKinds #-}

module Pitboss.Simulation.Event where

import Data.Aeson.Types
import GHC.Generics (Generic)
import Pitboss.Blackjack hiding (Hand)
import Pitboss.Causality

data BlackjackEvent
    = CardDealt Card HandId
    | ContestantStood ContestantId HandId
    | ContestantHit ContestantId HandId
    | ContestantDoubledDown ContestantId HandId
    | ContestantSplit ContestantId HandId
    | ContestantSurrendered ContestantId HandId
    | BoutSettled BoutId DetailedOutcome
    | DealerRevealed DealerId HandId
    | DealerHit DealerId HandId
    | DealerStood DealerId HandId
    deriving (Eq, Show, Generic)

instance ToJSON BlackjackEvent
instance FromJSON BlackjackEvent

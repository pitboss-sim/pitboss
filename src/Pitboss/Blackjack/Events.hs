{-# LANGUAGE DataKinds #-}

module Pitboss.Blackjack.Events where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.State.Types.Core
import Pitboss.Blackjack.Materia.Card
import Pitboss.Blackjack.Outcome

data BlackjackEvent
    = CardDealt Card CardDestination
    | PlayerStood (EntityId 'Player) (EntityId 'PlayerHand)
    | PlayerHit (EntityId 'Player) (EntityId 'PlayerHand)
    | PlayerDoubledDown (EntityId 'Player) (EntityId 'PlayerHand)
    | PlayerSplit (EntityId 'Player) (EntityId 'PlayerHand)
    | PlayerSurrender (EntityId 'Player) (EntityId 'PlayerHand)
    | BoutSettled (EntityId 'Bout) DetailedOutcome
    | DealerRevealed (EntityId 'Dealer) (EntityId 'DealerHand)
    deriving (Eq, Show, Generic)

data CardDestination
    = ToPlayerHand (EntityId 'PlayerHand)
    | ToDealerHand (EntityId 'DealerHand)
    deriving (Eq, Show, Generic)

instance ToJSON BlackjackEvent
instance FromJSON BlackjackEvent
instance ToJSON CardDestination
instance FromJSON CardDestination

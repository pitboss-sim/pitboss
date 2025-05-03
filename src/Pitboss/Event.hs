{-# LANGUAGE DeriveGeneric #-}

module Pitboss.Event where

import GHC.Generics (Generic)
import Pitboss.Blackjack.Card (Card)
import Pitboss.Blackjack.Chips (Chips)
import Pitboss.Identifier (PlayerId)
import Pitboss.Types.Index (HandIx, SpotIx)

data GameplayEvent
  = PlayerEvent PlayerAction
  | DealerEvent DealerAction
  | DerivedEvent Derived
  deriving (Eq, Show, Generic)

data PlayerAction
  = ClaimsSpot SpotIx
  | YieldsSpot SpotIx
  | PlacesBet SpotIx Chips
  | RemovesBet SpotIx
  | TakesInsurance SpotIx
  | DeclinesInsurance SpotIx
  | Hits SpotIx HandIx
  | Stands SpotIx HandIx
  | Doubles SpotIx HandIx
  | Splits SpotIx HandIx HandIx
  | Surrenders SpotIx HandIx
  | JoinsGame PlayerId
  | LeavesGame PlayerId
  | RequestsShuffle
  | -- | CausesDelay SpotIx Reason
    SignalsReady SpotIx
  deriving (Eq, Show, Generic)

data DealerAction
  = BeginsGame
  | EndsGame
  | ShufflesShoe
  | BurnsCard
  | SolicitsBets
  | DealsCard SpotIx HandIx Card
  | DealsHoleCard Card
  | RevealsHoleCard
  | DealsToSelf Card
  | OffersInsurance
  | RejectsInsurance
  deriving (Eq, Show, Generic)

data Derived
  = PlayerCompletesHand SpotIx HandIx PlayerReason
  | DealerCompletesHand DealerReason
  | SpotSettled SpotIx SettlementOutcome
  deriving (Eq, Show, Generic)

data PlayerReason
  = PlayerStand
  | PlayerBust
  | PlayerBlackjack
  | PlayerTwentyOne
  | PlayerSurrender
  | PlayerOneCardDraw
  deriving (Eq, Show, Generic)

data DealerReason
  = DealerStand
  | DealerBust
  | DealerBlackjack
  | DealerTwentyOne
  deriving (Eq, Show, Generic)

data SettlementOutcome
  = PlayerWins Chips
  | PlayerLoses
  | Push
  | BlackjackWin Chips
  | InsurancePaid Chips
  deriving (Eq, Show, Generic)

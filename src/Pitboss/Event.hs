module Pitboss.Event where

import Pitboss.Data

data GameplayEvent
  = Player PlayerAction
  | Dealer DealerAction

data PlayerAction
  = ClaimsSpot SpotId
  | YieldsSpot SpotId
  | PlacesBet SpotId Chips
  | RemovesBet SpotId
  | TakesInsurance SpotId
  | DeclinesInsurance SpotId
  | Hits SpotId HandId
  | Stands SpotId HandId
  | Doubles SpotId HandId
  | Splits SpotId HandId HandId
  | Surrenders SpotId HandId
  | CompletesHand SpotId HandId PlayerCompletionReason
  | JoinsGame PlayerId
  | LeavesGame PlayerId
  | RequestsShuffle
  | CausesDelay SpotId Reason
  | SignalsReady SpotId

data DealerAction
  = ShufflesShoe
  | BurnsCard
  | SolicitsBets
  | DealsFirstCard SpotId HandId Card
  | DealsSecondCard SpotId HandId Card
  | DealsHoleCard Card
  | RevealsHoleCard
  | DealsToSelf Card
  | HitsSelf Card
  | StandsSelf
  | CompletesHand DealerCompletionReason
  | OffersInsurance
  | RejectsInsurance
  | SettlesChips [(SpotId, SettlementOutcome)]
  | BeginsGame
  | EndsGame

data PlayerCompletionReason
  = PlayerStand
  | PlayerBust
  | PlayerBlackjack
  | PlayerTwentyOne
  | PlayerSurrender
  | PlayerOneCardDraw

data DealerCompletionReason
  = DealerStand
  | DealerBust
  | DealerBlackjack
  | DealerTwentyOne

data SettlementOutcome
  = PlayerWins Chips
  | PlayerLoses
  | Push
  | BlackjackWin Chips
  | InsurancePaid Chips

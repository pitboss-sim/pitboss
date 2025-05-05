module Pitboss.Event.Derive where

import Pitboss.Blackjack.Card (value)
import Pitboss.Event
import Pitboss.Table
import Pitboss.TableState

evaluateDerivedEvents :: TableState -> GameplayEvent -> [GameplayEvent]
evaluateDerivedEvents state ev = case ev of
  PlayerEvent (Hits spot hand) ->
    case handTotal state spot hand of
      Just 21 -> [DerivedEvent (PlayerCompletesHand spot hand PlayerTwentyOne)]
      Just n | n > 21 -> [DerivedEvent (PlayerCompletesHand spot hand PlayerBust)]
      _ -> []
  PlayerEvent (Stands spot hand) ->
    [DerivedEvent (PlayerCompletesHand spot hand PlayerStand)]
  PlayerEvent (Surrenders spot hand) ->
    [DerivedEvent (PlayerCompletesHand spot hand PlayerSurrender)]
  DealerEvent (DealsToSelf card) ->
    case dealerTotal (dealerHand state) of
      Just 21 -> [DerivedEvent (DealerCompletesHand DealerTwentyOne)]
      Just n | n > 21 -> [DerivedEvent (DealerCompletesHand DealerBust)]
      Just n
        | n >= 17 ->
            if dealerHitsSoft17 state && isSoftTotal state
              then []
              else [DerivedEvent (DealerCompletesHand DealerStand)]
      _ -> []
  _ -> []

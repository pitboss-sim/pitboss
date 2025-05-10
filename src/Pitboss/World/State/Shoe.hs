module Pitboss.World.State.Shoe where

import Pitboss.Blackjack.Card (Card)
import Pitboss.World.State.Types.Clocked
import Pitboss.World.State.Types.DeltaDriven

data ShoeState = ShoeState
  { shoeTick :: Tick,
    cardsRemaining :: [Card],
    cutPoint :: Maybe Int
  }
  deriving (Eq, Show)

data ShoeDelta
  = DrawCard Card
  | SetCutPoint (Maybe Int)
  | RefillShoe [Card]
  deriving (Eq, Show)

instance Clocked ShoeState where
  tick = shoeTick
  setTick t ss = ss {shoeTick = t}

instance DeltaDriven ShoeState ShoeDelta where
  applyDelta d ss = case d of
    DrawCard _ -> ss {cardsRemaining = drop 1 (cardsRemaining ss)}
    SetCutPoint cp -> ss {cutPoint = cp}
    RefillShoe newDeck -> ss {cardsRemaining = newDeck}

  describeDelta :: ShoeDelta -> entity -> String
  describeDelta d _ = case d of
    DrawCard c -> "Drew card: " ++ show c
    SetCutPoint cp -> "Set cut point to " ++ maybe "Nothing" show cp
    RefillShoe cards -> "Refilled shoe with " ++ show (length cards) ++ " cards"

  previewDelta d ss = Just (applyDelta d ss)

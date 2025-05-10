module Pitboss.World.State.Shoe where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Card (Card)
import Pitboss.World.State.Types.Clocked
import Pitboss.World.State.Types.DeltaDriven

mkShoeState :: Tick -> [Card] -> Maybe Int -> ShoeState
mkShoeState t cards cut =
  ShoeState
    { _tick = t,
      _cardsRemaining = cards,
      _cutPoint = cut
    }

data ShoeState = ShoeState
  { _tick :: Tick,
    _cardsRemaining :: [Card],
    _cutPoint :: Maybe Int
  }
  deriving (Eq, Show, Generic)

instance ToJSON ShoeState

instance FromJSON ShoeState

data ShoeDelta
  = DrawCard Card
  | SetCutPoint (Maybe Int)
  | RefillShoe [Card]
  deriving (Eq, Show, Generic)

instance ToJSON ShoeDelta

instance FromJSON ShoeDelta

instance Clocked ShoeState where
  tick = _tick
  setTick t ss = ss {_tick = t}

instance DeltaDriven ShoeState ShoeDelta where
  applyDelta d ss = case d of
    DrawCard _ -> ss {_cardsRemaining = drop 1 (_cardsRemaining ss)}
    SetCutPoint cp -> ss {_cutPoint = cp}
    RefillShoe newDeck -> ss {_cardsRemaining = newDeck}

  describeDelta :: ShoeDelta -> entity -> String
  describeDelta d _ = case d of
    DrawCard c -> "Drew card: " ++ show c
    SetCutPoint cp -> "Set cut point to " ++ maybe "Nothing" show cp
    RefillShoe cards -> "Refilled shoe with " ++ show (length cards) ++ " cards"

  previewDelta d ss = Just (applyDelta d ss)

module Pitboss.Trace.Delta.Shoe where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Card (Card)
import Pitboss.Trace.Delta.Types.DeltaDriven
import Pitboss.Trace.Entity.Shoe

data ShoeDelta
  = DrawCard Card
  | SetCutPoint (Maybe Int)
  | RefillShoe [Card]
  deriving (Eq, Show, Generic)

instance ToJSON ShoeDelta

instance FromJSON ShoeDelta

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

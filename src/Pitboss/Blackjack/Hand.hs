module Pitboss.Blackjack.Hand
  ( maxCardsFor,
    mkHand,
  )
where

import Pitboss.Blackjack.Card (Card)
import Pitboss.Offering.Matter (DeckCount (..), Matter (matterDecks))

newtype CutPoint = CutPoint Int
  deriving (Show, Eq)

newtype Hand = Hand [Card]
  deriving (Eq, Show)

-- helper
maxCardsFor :: DeckCount -> Int
maxCardsFor d = case d of
  D1 -> 11
  D2 -> 14
  D6 -> 21
  D8 -> 21

mkHand :: Matter -> [Card] -> Maybe Hand
mkHand mat cs
  | length cs <= maxCardsFor (matterDecks mat) = Just (Hand cs)
  | otherwise = Nothing

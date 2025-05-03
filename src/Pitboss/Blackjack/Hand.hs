module Pitboss.Blackjack.Hand
  ( mkHand,
    Hand (..),
    module Pitboss.Blackjack.Hand.Category,
  )
where

import Pitboss.Blackjack.Card (Card)
import Pitboss.Blackjack.Hand.Category
import Pitboss.Blackjack.Offering.Matter (DeckCount (..), Matter (matterDecks))

newtype Hand = Hand [Card]
  deriving (Eq, Show)

mkHand :: Matter -> [Card] -> Maybe Hand
mkHand mat cs
  | length cs <= maxCardsFor (matterDecks mat) = Just (Hand cs)
  | otherwise = Nothing
  where
    maxCardsFor :: DeckCount -> Int
    maxCardsFor d = case d of
      D1 -> 11
      D2 -> 14
      D6 -> 21
      D8 -> 21

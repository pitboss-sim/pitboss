module Pitboss.Blackjack.Hand.Score where

import Pitboss.Blackjack.Card (Card (..), Rank (..))
import Pitboss.Blackjack.Hand (Hand (..))
import Pitboss.Blackjack.Hand.Category (isBlackjack)

data DealerScore
  = DealerBlackjack
  | Total Int Bool -- value, isSoft
  deriving (Eq, Show)

dealerHandTotal :: Hand -> Maybe DealerScore
dealerHandTotal (Hand cards)
  | isBlackjack cards = Just DealerBlackjack
  | otherwise =
      let base = hardHandTotal cards
          hasAce = any ((== Ace) . rank) cards
          soft = hasAce && base + 10 <= 21
          total = if soft then base + 10 else base
       in if total > 21
            then Nothing
            else Just (Total total soft)

data Score
  = Busted
  | Blackjack
  | Valued Int
  deriving (Eq, Show)

isSoft :: Hand -> Bool
isSoft (Hand cards) =
  case softHandTotal cards of
    Just softTotal -> hardHandTotal cards /= softTotal
    Nothing -> False

rankValue :: Rank -> Int
rankValue Ace = 1
rankValue r
  | r `elem` [Ten, Jack, Queen, King] = 10
  | otherwise = fromEnum r + 2

hardHandTotal :: [Card] -> Int
hardHandTotal = sum . map (rankValue . rank)

softHandTotal :: [Card] -> Maybe Int
softHandTotal cards =
  let base = hardHandTotal cards
      hasAce = any ((== Ace) . rank) cards
   in if hasAce && base + 10 <= 21
        then Just (base + 10)
        else
          if base <= 21
            then Just base
            else Nothing

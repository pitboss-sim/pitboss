module Pitboss.Blackjack.Hand.Score where

import Pitboss.Blackjack.Card (Card (..), Rank (..))
import Pitboss.Blackjack.Hand (Hand (..))

data Score
  = Busted
  | Blackjack
  | Valued Int
  deriving (Eq, Show)

scoreHand :: Hand -> Score
scoreHand (Hand cards) =
  case softHandTotal cards of
    Nothing -> Busted
    Just 21 | length cards == 2 -> Blackjack
    Just val -> Valued val

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

module Pitboss.Blackjack.Hand.Score where

import Pitboss.Blackjack.Card (Card (..), Rank (..))
import Pitboss.Blackjack.Hand (Hand (..))

data Score
  = Busted
  | Blackjack
  | Valued Int
  deriving (Eq, Show)

scoreHand :: Hand -> Score
scoreHand (Hand cards)
  | length cards == 2 && total == 21 = Blackjack
  | total > 21 = Busted
  | otherwise = Valued total
  where
    total = handValue (Hand cards)

isSoft :: Hand -> Bool
isSoft (Hand cards) = go (0 :: Int) (0 :: Int) cards
  where
    go acc aces [] = aces > 0 && acc + 10 <= 21
    go acc aces (Card r _ : cs) =
      case r of
        Ace -> go (acc + 1) (aces + 1) cs
        _ -> go (acc + rankValue r) aces cs

handValue :: Hand -> Int
handValue (Hand cards) = adjust base aceCount
  where
    (base, aceCount) = foldr step (0 :: Int, 0 :: Int) cards
    step (Card r _) (acc, aces) =
      case r of
        Ace -> (acc + 1, aces + 1)
        _ -> (acc + rankValue r, aces)

    adjust total _
      | total + 10 <= 21 = total + 10
      | otherwise = total

rankValue :: Rank -> Int
rankValue Ace = 1
rankValue r
  | r `elem` [Ten, Jack, Queen, King] = 10
  | otherwise = fromEnum r + 2

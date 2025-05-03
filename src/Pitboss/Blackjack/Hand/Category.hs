module Pitboss.Blackjack.Hand.Category where

import Data.List (sort)
import Pitboss.Blackjack.Card (Card (..), Rank (..), value)

newtype HandRanks = HandRanks {getHandRanks :: [Rank]}
  deriving (Eq, Show, Ord)

mkHandRanks :: [Rank] -> HandRanks
mkHandRanks = HandRanks . sort

mkHandRanksFromCards :: [Card] -> HandRanks
mkHandRanksFromCards = mkHandRanks . map rank

data HandCategory
  = Empty
  | Partial Rank
  | Blackjack
  | TwentyOne
  | Pair Rank
  | Soft2 Rank
  | SoftN HandRanks
  | Hard2 Rank Rank
  | HardN HandRanks
  deriving (Eq, Show)

categorize :: [Card] -> HandCategory
categorize [] = Empty
categorize [c] = Partial (rank c)
categorize cards
  | isBlackjack cards = Blackjack
  | total == 21 = TwentyOne
  | isPair cards = Pair (rank (head cards))
  | isSoft = case nonAces of
      [c] -> Soft2 (rank c)
      _ -> SoftN (mkHandRanksFromCards nonAces)
  | otherwise = case map rank cards of
      [r1, r2] -> Hard2 r1 r2
      rs -> HardN (mkHandRanks rs)
  where
    ranks = map rank cards
    values = map value cards
    totalRaw = sum values
    numAces = length (filter (== Ace) ranks)
    total = adjustForAces totalRaw numAces
    isSoft = numAces > 0 && total <= 21
    nonAces = filter ((/= Ace) . rank) cards

adjustForAces :: Int -> Int -> Int
adjustForAces total numAces
  | total <= 21 = total
  | numAces == 0 = total
  | otherwise = adjustForAces (total - 10) (numAces - 1)

isBlackjack :: [Card] -> Bool
isBlackjack [Card Ace _, Card r _] = isTenValued r
isBlackjack [Card r _, Card Ace _] = isTenValued r
isBlackjack _ = False

isTenValued :: Rank -> Bool
isTenValued r = r `elem` [Ten, Jack, Queen, King]

isPair :: [Card] -> Bool
isPair [c1, c2] = rank c1 == rank c2
isPair _ = False

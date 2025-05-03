module Pitboss.Blackjack.Card where

data Suit = Hearts | Diamonds | Clubs | Spades
  deriving (Show, Eq, Enum)

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Show, Eq, Enum, Ord)

data Card = Card {rank :: Rank, suit :: Suit}
  deriving (Show, Eq)

value :: Card -> Int
value (Card Two _) = 2
value (Card Three _) = 3
value (Card Four _) = 4
value (Card Five _) = 5
value (Card Six _) = 6
value (Card Seven _) = 7
value (Card Eight _) = 8
value (Card Nine _) = 9
value (Card Ten _) = 10
value (Card Jack _) = 10
value (Card Queen _) = 10
value (Card King _) = 10
value (Card Ace _) = 11

cardValue :: Card -> Int
cardValue = rankValue . rank

rankValue :: Rank -> Int
rankValue r = case r of
  Two -> 2
  Three -> 3
  Four -> 4
  Five -> 5
  Six -> 6
  Seven -> 7
  Eight -> 8
  Nine -> 9
  Ten -> 10
  Jack -> 10
  Queen -> 10
  King -> 10
  Ace -> 11

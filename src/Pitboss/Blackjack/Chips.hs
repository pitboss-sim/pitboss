module Pitboss.Blackjack.Chips
  ( Chips (..),
  )
where

newtype Chips = Chips Int
  deriving (Eq, Ord, Show, Num)

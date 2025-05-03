module Pitboss.Blackjack.Hand.Types
  ( HandRanks (..),
  )
where

import Pitboss.Blackjack.Card (Rank)

newtype HandRanks = HandRanks {getHandRanks :: [Rank]}
  deriving (Eq, Show, Ord)

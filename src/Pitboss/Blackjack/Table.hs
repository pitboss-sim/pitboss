module Pitboss.Blackjack.Table
  ( allSeats,
    Table (..),
  )
where

import Pitboss.Blackjack.Types.Index (SeatIx)

allSeats :: [SeatIx]
allSeats = [minBound .. maxBound]

data Table where
  Table :: {tableSeats :: [SeatIx]} -> Table
  deriving (Eq, Show)

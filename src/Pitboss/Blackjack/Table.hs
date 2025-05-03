module Pitboss.Blackjack.Table
  ( allSeats,
    Table (..),
    TableSeatIx,
  )
where

import Pitboss.Blackjack.Types.Index (SeatIx)

data TableSeatIx = Seat1 | Seat2 | Seat3 | Seat4 | Seat5 | Seat6
  deriving (Eq, Ord, Enum, Bounded, Show)

instance BoundedEnum SeatIx

allSeats :: [SeatIx]
allSeats = [minBound .. maxBound]

data Table where
  Table :: {tableSeats :: [SeatIx]} -> Table
  deriving (Eq, Show)

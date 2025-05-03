module Pitboss.Blackjack.Table where

import Pitboss.Types.BoundedEnum (BoundedEnum)

data TableSeatIx = Seat1 | Seat2 | Seat3 | Seat4 | Seat5 | Seat6
  deriving (Eq, Ord, Enum, Bounded, Show)

instance BoundedEnum TableSeatIx

allSeats :: [TableSeatIx]
allSeats = [minBound .. maxBound]

data Table where
  Table :: {tableSeats :: [TableSeatIx]} -> Table
  deriving (Eq, Show)

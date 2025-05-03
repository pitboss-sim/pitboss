module Pitboss.Blackjack.Table
  ( SeatIx (..),
    allSeats,
    Table (..),
  )
where

import Pitboss.Types.BoundedEnum (BoundedEnum)
import Pitboss.Types.Index (SeatIx)

allSeats :: [SeatIx]
allSeats = [minBound .. maxBound]

data Table where
  Table :: {tableSeats :: [SeatIx]} -> Table
  deriving (Eq, Show)

module Pitboss.Types.Index
  ( SeatIx,
  )
where

import Pitboss.Types.BoundedEnum (BoundedEnum)

data SeatIx = Seat1 | Seat2 | Seat3 | Seat4 | Seat5 | Seat6
  deriving (Eq, Ord, Enum, Bounded, Show)

instance BoundedEnum SeatIx

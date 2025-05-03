module Pitboss.Blackjack.Types.Index
  ( SeatIx,
    PlayerHandIx,
  )
where

import Pitboss.Types.BoundedEnum (BoundedEnum)

data SeatIx = Seat1 | Seat2 | Seat3 | Seat4 | Seat5 | Seat6
  deriving (Eq, Ord, Enum, Bounded, Show)

instance BoundedEnum SeatIx

data PlayerHandIx
  = PlayerHand1
  | PlayerHand2
  | PlayerHand3
  | PlayerHand4
  deriving (Eq, Ord, Enum, Bounded, Show)

instance BoundedEnum PlayerHandIx

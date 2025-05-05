module Pitboss.Sim.State.Player where

import Pitboss.Types.BoundedEnum (BoundedEnum)

-- Up to four spots played by one player simultaneously
data PlayerSpotIx = PlayerSpot1 | PlayerSpot2 | PlayerSpot3 | PlayerSpot4
  deriving (Eq, Ord, Enum, Bounded, Show)

instance BoundedEnum PlayerSpotIx

-- Up to sixteen spot hands (4 spot x 4 hand) played by player simulataneously
data PlayerSpotHandIx
  = Hand1
  | Hand2
  | Hand3
  | Hand4
  | Hand5
  | Hand6
  | Hand7
  | Hand8
  | Hand9
  | Hand10
  | Hand11
  | Hand12
  | Hand13
  | Hand14
  | Hand15
  | Hand16
  deriving (Eq, Ord, Enum, Bounded, Show)

instance BoundedEnum PlayerSpotHandIx

data PlayerState where
  PlayerState ::
    {
    } ->
    PlayerState

module Pitboss.Sim.State.Player where

import Pitboss.Types.BoundedEnum (BoundedEnum)

-- Up to four spots played by one player simultaneously
data PlayerSpotIx = PlayerSpot1 | PlayerSpot2 | PlayerSpot3 | PlayerSpot4
  deriving (Eq, Ord, Enum, Bounded, Show)

instance BoundedEnum PlayerSpotIx

-- Up to sixteen spot hands (4 spot x 4 hand) played by player simulataneously
data PlayerSpotHandIx = Hand1 | Hand2 | Hand3 | Hand4
  deriving (Eq, Ord, Enum, Bounded, Show)

instance BoundedEnum PlayerSpotHandIx

data PlayerState where
  PlayerState ::
    {
    } ->
    PlayerState

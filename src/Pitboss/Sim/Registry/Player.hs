module Pitboss.Sim.State.PlayerRegistry where

import qualified Data.Map.Strict as Map
import Pitboss.Sim.Types.Identifier (PlayerId)

newtype PlayerRegistry = PlayerRegistry (Map.Map PlayerId PlayerMeta)
  deriving (Eq, Show)

data PlayerMeta = PlayerMeta
  { playerName :: String
  }

emptyRegistry :: PlayerRegistry
emptyRegistry = PlayerRegistry Map.empty

addPlayer :: PlayerId -> PlayerMeta -> PlayerRegistry -> PlayerRegistry
addPlayer pid meta (PlayerRegistry m) = PlayerRegistry (Map.insert pid meta m)

getPlayer :: PlayerId -> PlayerRegistry -> Maybe PlayerMeta
getPlayer pid (PlayerRegistry m) = Map.lookup pid m

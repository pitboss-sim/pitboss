module Pitboss.World.State.Types.Snapshot where

import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Pitboss.World.State.Types.Clocked (Tick)

data StateSnapshot entity delta = StateSnapshot
  { entity :: entity,
    history :: InsOrdHashMap Tick delta
  }

latestSnapshot :: StateSnapshot e d -> e
latestSnapshot = entity

module Pitboss.Registry.State.Types.Snapshot where

import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Pitboss.Registry.State.Types.Clocked (Tick)

data StateSnapshot entity delta = StateSnapshot
  { entity :: entity,
    history :: InsOrdHashMap Tick delta
  }

latestSnapshot :: StateSnapshot e d -> e
latestSnapshot = entity

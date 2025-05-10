module Pitboss.Registry.State.Types.Snapshot where

import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.Word (Word64)

data StateSnapshot entity delta = StateSnapshot
  { entity :: entity,
    history :: InsOrdHashMap Word64 delta
  }

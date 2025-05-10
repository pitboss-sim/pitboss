module Pitboss.World.State.Types.Snapshot where

import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.HashMap.Strict.InsOrd qualified as IHM
import Pitboss.World.State.Types.Clocked (Clocked (..), Tick)
import Pitboss.World.State.Types.DeltaDriven (DeltaDriven (..))

data StateSnapshot entity delta = StateSnapshot
  { entity :: entity,
    history :: InsOrdHashMap Tick delta
  }

latestSnapshot :: StateSnapshot e d -> e
latestSnapshot = entity

applySnapshotDelta ::
  (DeltaDriven entity delta, Clocked entity) =>
  (Tick -> Tick) ->
  delta ->
  StateSnapshot entity delta ->
  StateSnapshot entity delta
applySnapshotDelta bumpTick delta (StateSnapshot entity history) =
  let oldTick = tick entity
      newTick = bumpTick oldTick
      updatedEntity = applyDelta delta entity
      updatedHistory = IHM.insert newTick delta history
   in StateSnapshot
        { entity = setTick newTick updatedEntity,
          history = updatedHistory
        }

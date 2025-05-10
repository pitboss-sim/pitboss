module Pitboss.World.State.Types.Snapshot where

import Data.Aeson (FromJSON, ToJSON)
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.HashMap.Strict.InsOrd qualified as IHM
import GHC.Generics (Generic)
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

data EntityRef e = EntityRef
  { refTick :: Tick,
    refEntity :: e
  }
  deriving (Eq, Show, Generic)

instance (ToJSON e) => ToJSON (EntityRef e)

instance (FromJSON e) => FromJSON (EntityRef e)

emptyHistory :: InsOrdHashMap Tick delta
emptyHistory = IHM.empty

defaultSnapshot :: (Clocked e) => e -> StateSnapshot e d
defaultSnapshot e = StateSnapshot e emptyHistory

reconstructAtTick ::
  (Clocked e, DeltaDriven e d) =>
  StateSnapshot e d ->
  Tick ->
  Maybe (EntityRef e)
reconstructAtTick (StateSnapshot base hist) t = do
  let deltasToApply = IHM.toList (IHM.filterWithKey (\k _ -> k > tick base && k <= t) hist)
  let result = foldl (\e (_, d) -> applyDelta d e) base deltasToApply
  pure $ EntityRef t (setTick t result)

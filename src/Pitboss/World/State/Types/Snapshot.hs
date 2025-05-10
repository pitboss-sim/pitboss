{-# LANGUAGE OverloadedStrings #-}

module Pitboss.World.State.Types.Snapshot where

import Data.Aeson (FromJSON, KeyValue ((.=)), ToJSON, object, parseJSON, toJSON, withObject, (.:))
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.HashMap.Strict.InsOrd qualified as IHM
import GHC.Generics (Generic)
import Pitboss.World.State.Types.Clocked (Clocked (..), Tick)
import Pitboss.World.State.Types.DeltaDriven (DeltaDriven (..))
import Pitboss.World.Types.EntityRef (EntityRef (..))

data StateSnapshot entity delta = StateSnapshot
  { entity :: entity,
    history :: InsOrdHashMap Tick delta
  }
  deriving (Eq, Show, Generic)

instance (ToJSON entity, ToJSON delta) => ToJSON (StateSnapshot entity delta) where
  toJSON (StateSnapshot e h) =
    object
      [ "entity" .= e,
        "history" .= h
      ]

instance (FromJSON entity, FromJSON delta) => FromJSON (StateSnapshot entity delta) where
  parseJSON = withObject "StateSnapshot" $ \o ->
    StateSnapshot
      <$> o .: "entity"
      <*> o .: "history"

mkSnapshot :: (Clocked e) => e -> StateSnapshot e d
mkSnapshot e = StateSnapshot e emptyHistory

emptyHistory :: InsOrdHashMap Tick delta
emptyHistory = IHM.empty

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

reconstructAtTick ::
  (Clocked e, DeltaDriven e d) =>
  StateSnapshot e d ->
  Tick ->
  Maybe (EntityRef e)
reconstructAtTick (StateSnapshot base hist) t = do
  let deltasToApply = IHM.toList (IHM.filterWithKey (\k _ -> k > tick base && k <= t) hist)
  let result = foldl (\e (_, d) -> applyDelta d e) base deltasToApply
  pure $ EntityRef t (setTick t result)

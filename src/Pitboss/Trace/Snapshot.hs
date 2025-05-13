{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Pitboss.Trace.Snapshot where

import Data.Aeson (FromJSON, KeyValue ((.=)), ToJSON, object, parseJSON, toJSON, withObject, (.:))
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.HashMap.Strict.InsOrd qualified as IHM
import GHC.Generics (Generic)
import Pitboss.Trace.Entity.Capabilities.Clocked
import Pitboss.Trace.Entity.Capabilities.DeltaDriven

type Timeline delta = InsOrdHashMap Tick [delta]

data StateSnapshot entity = StateSnapshot
  { entity :: entity,
    timeline :: Timeline (Delta entity)
  }
  deriving (Generic)

deriving instance (Eq entity, Eq (Delta entity)) => Eq (StateSnapshot entity)

deriving instance (Show entity, Show (Delta entity)) => Show (StateSnapshot entity)

instance (ToJSON entity, ToJSON (Delta entity)) => ToJSON (StateSnapshot entity) where
  toJSON (StateSnapshot e h) =
    object
      [ "entity" .= e,
        "history" .= h
      ]

instance (FromJSON entity, FromJSON (Delta entity)) => FromJSON (StateSnapshot entity) where
  parseJSON = withObject "StateSnapshot" $ \o ->
    StateSnapshot
      <$> o .: "entity"
      <*> o .: "history"

mkSnapshot ::
  (DeltaDriven entity) =>
  entity ->
  Tick ->
  [Delta entity] ->
  StateSnapshot entity
mkSnapshot ent tick' deltas =
  StateSnapshot (applyDeltas ent deltas) (IHM.singleton tick' deltas)

emptyHistory :: InsOrdHashMap Tick delta
emptyHistory = IHM.empty

latestSnapshot :: StateSnapshot e -> e
latestSnapshot = entity

-- timeline modification

emptyTimeline :: Timeline v
emptyTimeline = IHM.empty

applyDeltas :: (DeltaDriven entity) => entity -> [Delta entity] -> entity
applyDeltas = foldl (flip applyDelta)

updateSnapshot ::
  (DeltaDriven entity) =>
  Tick ->
  [Delta entity] ->
  StateSnapshot entity ->
  StateSnapshot entity
updateSnapshot tick' deltas (StateSnapshot e timeline) =
  let e' = applyDeltas e deltas
      t' = IHM.insertWith (++) tick' deltas timeline
   in StateSnapshot e' t'

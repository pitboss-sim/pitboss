{-# LANGUAGE OverloadedStrings #-}

module Pitboss.Trace.Snapshot where

import Data.Aeson (FromJSON, KeyValue ((.=)), ToJSON, object, parseJSON, toJSON, withObject, (.:))
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.HashMap.Strict.InsOrd qualified as IHM
import GHC.Generics (Generic)
import Pitboss.Trace.Entity.Capabilities.Clocked

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

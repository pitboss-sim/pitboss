{-# LANGUAGE TypeFamilyDependencies #-}

module Pitboss.Trace.Entity.Capabilities where

import Data.Aeson
import Data.Hashable
import Data.Word
import Pitboss.Trace.Types.Uid

-- delta application

class Incremental delta where
  type Entity delta = result | result -> delta

  applyDelta :: delta -> Entity delta -> Entity delta
  previewDelta :: delta -> Entity delta -> Maybe (Entity delta)
  describeDelta :: delta -> Entity delta -> String

class (Incremental delta) => Identifiable delta where
  entityToId :: delta -> Entity delta -> Uid

newtype Tick = Tick Word64
  deriving (Eq, Ord, Show, Hashable, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

class Reversible d where
  invert :: d -> Either InversionError d

data InversionError
  = NotInvertible
  | MissingPriorContext String
  | CustomReason String
  deriving (Eq, Show)

-- time

class Clocked entity where
  tick :: entity -> Tick
  setTick :: Tick -> entity -> entity

{-# LANGUAGE TypeFamilyDependencies #-}

module Pitboss.Trace.Entity.Capabilities where

import Data.Aeson
import Data.Hashable
import Data.Word
import Pitboss.Trace.Types.Uid

-- delta application

class Incremental delta where
    type Target delta = target | target -> delta

    applyDelta :: delta -> Target delta -> Target delta
    previewDelta :: delta -> Target delta -> Maybe (Target delta)
    describeDelta :: delta -> Target delta -> String

class (Incremental delta) => Identifiable delta where
    entityToId :: delta -> Target delta -> Uid

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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Pitboss.Trace.Delta.Types.Clocked where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Hashable (Hashable)
import Data.Word (Word64)

newtype Tick = Tick Word64
  deriving (Eq, Ord, Show, Hashable, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

class Clocked entity where
  tick :: entity -> Tick
  setTick :: Tick -> entity -> entity

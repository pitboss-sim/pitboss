module Pitboss.Registry.State.Types.Clocked where

import Data.Word (Word64)

class Clocked entity where
  tick :: entity -> Word64
  setTick :: Word64 -> entity -> entity

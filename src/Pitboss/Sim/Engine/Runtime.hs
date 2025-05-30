module Pitboss.Sim.Engine.Runtime where

import Data.Word (Word64)
import Pitboss.State.Types.Core

-- Helper to extract Word64 from Tick
unTick :: Tick -> Word64
unTick (Tick w) = w

-- Simple tick advancement - the actual simulation logic will be elsewhere
advanceTick :: Tick -> Tick
advanceTick (Tick w) = Tick (w + 1)

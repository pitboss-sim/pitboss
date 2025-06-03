module Pitboss.Sim.Engine.Runtime where

import Data.Word (Word64)
import Pitboss.State

unTick :: Tick -> Word64
unTick (Tick w) = w

advanceTick :: Tick -> Tick
advanceTick (Tick w) = Tick (w + 1)

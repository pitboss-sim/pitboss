module Pitboss.Simulation.Engine.Runtime where

import Data.Word (Word64)
import Pitboss.Causality

unTick :: Tick -> Word64
unTick (Tick w) = w

advanceTick :: Tick -> Tick
advanceTick (Tick w) = Tick (w + 1)

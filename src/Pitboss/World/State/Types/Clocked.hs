module Pitboss.World.State.Types.Clocked where

import Data.Aeson (FromJSON, ToJSON)
import Data.Hashable (Hashable)
import Data.Word (Word64)
import GHC.Generics (Generic)

newtype Tick = Tick {unTick :: Word64}
  deriving (Eq, Ord, Show, Enum, Num, Real, Integral, Generic)

instance ToJSON Tick

instance FromJSON Tick

instance Hashable Tick

class Clocked entity where
  tick :: entity -> Tick
  setTick :: Tick -> entity -> entity

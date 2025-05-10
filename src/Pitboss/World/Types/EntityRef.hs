module Pitboss.World.Types.EntityRef where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.World.State.Types.Clocked (Tick)

data EntityRef e = EntityRef
  { refTick :: Tick,
    refEntity :: e
  }
  deriving (Eq, Show, Generic)

instance (ToJSON e) => ToJSON (EntityRef e)

instance (FromJSON e) => FromJSON (EntityRef e)

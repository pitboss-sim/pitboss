{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Pitboss.Trace.Types.EntityRef where

import Data.Aeson
import GHC.Generics
import Pitboss.Trace.Capabilities.Clocked (Tick)

data EntityRef id where
  EntityRef ::
    { refTick :: Tick,
      refIdentifier :: id
    } ->
    EntityRef id

deriving instance (Eq id) => Eq (EntityRef id)

deriving instance (Show id) => Show (EntityRef id)

deriving instance (Generic id) => Generic (EntityRef id)

instance (ToJSON id, Generic id) => ToJSON (EntityRef id)

instance (FromJSON id, Generic id) => FromJSON (EntityRef id)

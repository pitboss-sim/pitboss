module Pitboss.Trace.Entity.Types.Meta where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Trace.Capabilities.Clocked
import Pitboss.Trace.Types.Uid

data Meta id = Meta
  { _id :: id,
    _bornAt :: Maybe Tick
  }
  deriving (Eq, Show, Generic)

instance (ToJSON id) => ToJSON (Meta id)

instance (FromJSON id) => FromJSON (Meta id)

instance (HasUid id) => HasUid (Meta id) where
  getUid = getUid . _id

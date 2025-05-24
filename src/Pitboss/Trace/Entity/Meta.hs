{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.Meta where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Trace.Entity.Types
import Pitboss.Trace.Entity.Types.Id

data family Meta (k :: EntityKind)

data instance Meta k = Meta
    { _id :: EntityRef k
    , _bornAt :: Maybe Tick
    }
    deriving (Eq, Show, Generic)

instance FromJSON (Meta k)
instance ToJSON (Meta k)

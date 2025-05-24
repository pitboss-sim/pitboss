{-# LANGUAGE TypeFamilies #-}

module Pitboss.State.Entity.Meta where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.State.Entity.Types
import Pitboss.State.Entity.Types.Id

data family Meta (k :: EntityKind)

data instance Meta k = Meta
    { _id :: EntityRef k
    , _bornAt :: Maybe Tick
    }
    deriving (Eq, Show, Generic)

instance FromJSON (Meta k)
instance ToJSON (Meta k)

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Pitboss.Trace.Entity.PlayerSpot where

import Data.Aeson
import GHC.Generics (Generic)
import Pitboss.Trace.Entity.PlayerSpot.Types
import Pitboss.Trace.Entity.Types.Meta
import Pitboss.Trace.Types.EntityRef
import Pitboss.Trace.Types.Identifier

mkPlayerSpotEntity :: Meta (EntityRef PlayerSpotEntityId) -> PlayerSpotEntityAttrs -> PlayerSpotEntityModes -> PlayerSpotEntityRels -> PlayerSpotEntity
mkPlayerSpotEntity = PlayerSpotEntity

data PlayerSpotEntity = PlayerSpotEntity
    { _playerSpotEntityMeta :: Meta (EntityRef PlayerSpotEntityId)
    , _playerSpotEntityAttrs :: PlayerSpotEntityAttrs
    , _playerSpotEntityModes :: PlayerSpotEntityModes
    , _playerSpotEntityRels :: PlayerSpotEntityRels
    }
    deriving (Eq, Show, Generic)

instance ToJSON PlayerSpotEntity

instance FromJSON PlayerSpotEntity

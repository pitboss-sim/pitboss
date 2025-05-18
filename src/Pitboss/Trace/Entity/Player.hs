{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Pitboss.Trace.Entity.Player where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Trace.Entity.Player.Types
import Pitboss.Trace.Entity.Types.Meta
import Pitboss.Trace.Types.EntityRef
import Pitboss.Trace.Types.Identifier

mkPlayerEntity :: Meta (EntityRef PlayerEntityId) -> PlayerEntityAttrs -> PlayerEntityModes -> PlayerEntityRels -> PlayerEntity
mkPlayerEntity = PlayerEntity

data PlayerEntity = PlayerEntity
    { _playerEntityMeta :: Meta (EntityRef PlayerEntityId)
    , _playerEntityAttrs :: PlayerEntityAttrs
    , _playerEntityModes :: PlayerEntityModes
    , _playerEntityRels :: PlayerEntityRels
    }
    deriving (Show, Eq, Generic)

instance ToJSON PlayerEntity

instance FromJSON PlayerEntity

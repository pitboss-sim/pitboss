{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Pitboss.Trace.Entity.PlayerHand where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Trace.Entity.PlayerHand.Types
import Pitboss.Trace.Entity.Types.Meta
import Pitboss.Trace.Types.EntityRef
import Pitboss.Trace.Types.Identifier

mkPlayerHandEntity :: Meta (EntityRef PlayerHandEntityId) -> PlayerHandEntityAttrs -> PlayerHandEntityModes -> PlayerHandEntityRels -> PlayerHandEntity
mkPlayerHandEntity = PlayerHandEntity

data PlayerHandEntity = PlayerHandEntity
    { _playerHandEntityMeta :: Meta (EntityRef PlayerHandEntityId)
    , _playerHandEntityAttrs :: PlayerHandEntityAttrs
    , _playerHandEntityModes :: PlayerHandEntityModes
    , _playerHandEntityRels :: PlayerHandEntityRels
    }
    deriving (Eq, Show, Generic)

instance ToJSON PlayerHandEntity

instance FromJSON PlayerHandEntity

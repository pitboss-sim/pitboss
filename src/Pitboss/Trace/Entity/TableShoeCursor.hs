{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Pitboss.Trace.Entity.TableShoeCursor where

import Data.Aeson.Types
import GHC.Generics (Generic)
import Pitboss.Trace.Entity.TableShoeCursor.Types
import Pitboss.Trace.Entity.Types.Meta
import Pitboss.Trace.Types.EntityRef
import Pitboss.Trace.Types.Identifier

mkTableShoeCursor :: Meta (EntityRef TableShoeCursorEntityId) -> TableShoeCursorEntityAttrs -> TableShoeCursorEntityModes -> TableShoeCursorEntityRels -> TableShoeCursorEntity
mkTableShoeCursor = TableShoeCursorEntity

data TableShoeCursorEntity = TableShoeCursorEntity
    { _tableShoeCursorEntityMeta :: Meta (EntityRef TableShoeCursorEntityId)
    , _tableShoeCursorEntityAttrs :: TableShoeCursorEntityAttrs
    , _tableShoeCursorEntityModes :: TableShoeCursorEntityModes
    , _tableShoeCursorEntityRels :: TableShoeCursorEntityRels
    }
    deriving (Eq, Show, Generic)

instance ToJSON TableShoeCursorEntity

instance FromJSON TableShoeCursorEntity

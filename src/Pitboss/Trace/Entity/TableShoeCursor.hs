{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Pitboss.Trace.Entity.TableShoeCursor where

import Data.Aeson.Types
import GHC.Generics (Generic)
import Pitboss.Trace.Entity.Types.Meta
import Pitboss.Trace.Types.EntityRef
import Pitboss.Trace.Types.Identifier

mkTableShoeCursor :: Meta (EntityRef TableShoeCursorEntityId) -> TableShoeCursorEntityAttrs -> TableShoeCursorEntityModes -> TableShoeCursorEntityRels -> TableShoeCursorEntity
mkTableShoeCursor = TableShoeCursorEntity

mkTableShoeCursorEntityAttrs :: Int -> TableShoeCursorEntityAttrs
mkTableShoeCursorEntityAttrs = TableShoeCursorEntityAttrs

mkTableShoeCursorEntityModes :: TableShoeCursorEntityModes
mkTableShoeCursorEntityModes = TableShoeCursorEntityModes

mkTableShoeCursorEntityRels :: EntityRef TableShoeEntityId -> TableShoeCursorEntityRels
mkTableShoeCursorEntityRels = TableShoeCursorEntityRels

data TableShoeCursorEntity = TableShoeCursorEntity
    { _tableShoeCursorEntityMeta :: Meta (EntityRef TableShoeCursorEntityId)
    , _tableShoeCursorEntityAttrs :: TableShoeCursorEntityAttrs
    , _tableShoeCursorEntityModes :: TableShoeCursorEntityModes
    , _tableShoeCursorEntityRels :: TableShoeCursorEntityRels
    }
    deriving (Eq, Show, Generic)

data TableShoeCursorEntityAttrs = TableShoeCursorEntityAttrs
    { _tableShoeCursorEntityAttrsOffset :: Int
    }
    deriving (Eq, Show, Generic)

data TableShoeCursorEntityModes = TableShoeCursorEntityModes
    {
    }
    deriving (Eq, Show, Generic)

data TableShoeCursorEntityRels = TableShoeCursorEntityRels
    { _tableShoeCursorEntityRelsPointsToTableShoe :: EntityRef TableShoeEntityId
    }
    deriving (Eq, Show, Generic)

instance ToJSON TableShoeCursorEntity

instance FromJSON TableShoeCursorEntity

instance ToJSON TableShoeCursorEntityAttrs

instance FromJSON TableShoeCursorEntityAttrs

instance ToJSON TableShoeCursorEntityModes

instance FromJSON TableShoeCursorEntityModes

instance ToJSON TableShoeCursorEntityRels

instance FromJSON TableShoeCursorEntityRels

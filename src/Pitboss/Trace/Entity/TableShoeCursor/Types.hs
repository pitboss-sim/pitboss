{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Pitboss.Trace.Entity.TableShoeCursor.Types where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Trace.Types.EntityRef
import Pitboss.Trace.Types.Identifier

mkTableShoeCursorEntityAttrs :: Int -> TableShoeCursorEntityAttrs
mkTableShoeCursorEntityAttrs = TableShoeCursorEntityAttrs

mkTableShoeCursorEntityModes :: TableShoeCursorEntityModes
mkTableShoeCursorEntityModes = TableShoeCursorEntityModes

mkTableShoeCursorEntityRels :: EntityRef TableShoeEntityId -> TableShoeCursorEntityRels
mkTableShoeCursorEntityRels = TableShoeCursorEntityRels

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

instance ToJSON TableShoeCursorEntityAttrs

instance FromJSON TableShoeCursorEntityAttrs

instance ToJSON TableShoeCursorEntityModes

instance FromJSON TableShoeCursorEntityModes

instance ToJSON TableShoeCursorEntityRels

instance FromJSON TableShoeCursorEntityRels

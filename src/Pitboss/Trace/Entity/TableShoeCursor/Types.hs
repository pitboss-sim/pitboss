{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Pitboss.Trace.Entity.TableShoeCursor.Types where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Trace.Entity.Types
import Pitboss.Trace.Entity.Types.EntityId

mkTableShoeCursorEntityAttrs :: Int -> TableShoeCursorEntityAttrs
mkTableShoeCursorEntityAttrs = TableShoeCursorEntityAttrs

mkTableShoeCursorEntityModes :: TableShoeCursorEntityModes
mkTableShoeCursorEntityModes = TableShoeCursorEntityModes

mkTableShoeCursorEntityRels :: ClockedRef TableShoeEntityId -> TableShoeCursorEntityRels
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
    { _tableShoeCursorEntityRelsPointsToTableShoe :: ClockedRef TableShoeEntityId
    }
    deriving (Eq, Show, Generic)

instance ToJSON TableShoeCursorEntityAttrs

instance FromJSON TableShoeCursorEntityAttrs

instance ToJSON TableShoeCursorEntityModes

instance FromJSON TableShoeCursorEntityModes

instance ToJSON TableShoeCursorEntityRels

instance FromJSON TableShoeCursorEntityRels

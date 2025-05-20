module Pitboss.Trace.Entity.TableShoeCursor.Delta where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Trace.Entity.Types.EntityId

data TableShoeCursorEntityAttrsDelta
    = Advance Int
    | Rewind Int
    | ReplaceOffset Int Int
    deriving (Eq, Show, Generic)

instance ToJSON TableShoeCursorEntityAttrsDelta
instance FromJSON TableShoeCursorEntityAttrsDelta

data TableShoeCursorEntityModesDelta = NoopModes
    deriving (Eq, Show, Generic)

instance ToJSON TableShoeCursorEntityModesDelta
instance FromJSON TableShoeCursorEntityModesDelta

data TableShoeCursorEntityRelsDelta
    = UpdateTableShoe (ClockedRef TableShoeEntityId) (ClockedRef TableShoeEntityId)
    deriving (Eq, Show, Generic)

instance ToJSON TableShoeCursorEntityRelsDelta
instance FromJSON TableShoeCursorEntityRelsDelta

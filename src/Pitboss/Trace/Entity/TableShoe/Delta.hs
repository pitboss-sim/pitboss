{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Pitboss.Trace.Entity.TableShoe.Delta where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict (Map)
import Data.Void (Void)
import GHC.Generics (Generic)
import Pitboss.Trace.Entity.TableShoe.Entity
import Pitboss.Trace.Entity.Types.EntityId

data DTableShoeAttrs
    = DTableShoeSetCardStateMap (Map CardIx CardState) (Map CardIx CardState)
    | DTableShoeSetCardFate CardIx CardState
    deriving (Eq, Show, Generic)

data DTableShoeModes = DTableShoeModes Void
    deriving (Eq, Show, Generic)

data DTableShoeRels
    = DTableShoeSetTable (ClockedRef ETableId) (ClockedRef ETableId)
    deriving (Eq, Show, Generic)

instance ToJSON DTableShoeAttrs
instance FromJSON DTableShoeAttrs

instance ToJSON DTableShoeModes
instance FromJSON DTableShoeModes

instance ToJSON DTableShoeRels
instance FromJSON DTableShoeRels

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Pitboss.Trace.Entity.TableShoe where

import Data.Aeson
import GHC.Generics (Generic)
import Pitboss.Trace.Entity.TableShoe.Types
import Pitboss.Trace.Entity.Types.Meta
import Pitboss.Trace.Types.EntityRef
import Pitboss.Trace.Types.Identifier

mkTableShoeEntity :: Meta (EntityRef TableShoeEntityId) -> TableShoeEntityAttrs -> TableShoeEntityModes -> TableShoeEntityRels -> TableShoeEntity
mkTableShoeEntity = TableShoeEntity

data TableShoeEntity = TableShoeEntity
    { _shoeEntityMeta :: Meta (EntityRef TableShoeEntityId)
    , _shoeEntityAttrs :: TableShoeEntityAttrs
    , _shoeEntityModes :: TableShoeEntityModes
    , _shoeEntityRels :: TableShoeEntityRels
    }
    deriving (Eq, Show, Generic)

instance ToJSON TableShoeEntity

instance FromJSON TableShoeEntity

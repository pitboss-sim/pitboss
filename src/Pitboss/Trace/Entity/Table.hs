{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Pitboss.Trace.Entity.Table where

import Data.Aeson
import GHC.Generics hiding (Meta)
import Pitboss.Trace.Entity.Table.Types
import Pitboss.Trace.Entity.Types.Meta
import Pitboss.Trace.Types.Identifier

mkTableEntity :: Meta TableEntityId -> TableEntityAttrs -> TableEntityModes -> TableEntityRels -> TableEntity
mkTableEntity = TableEntity

data TableEntity = TableEntity
    { _tableEntityMeta :: Meta TableEntityId
    , _tableEntityAttrs :: TableEntityAttrs
    , _tableEntityModes :: TableEntityModes
    , _tableEntityRels :: TableEntityRels
    }
    deriving (Eq, Show, Generic)

instance ToJSON TableEntity

instance FromJSON TableEntity

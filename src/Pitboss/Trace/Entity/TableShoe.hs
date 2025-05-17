{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Pitboss.Trace.Entity.TableShoe where

import Data.Aeson
import GHC.Generics (Generic)
import Pitboss.Blackjack.Card (Card)
import Pitboss.Trace.Entity.Types.Meta
import Pitboss.Trace.Types.EntityRef
import Pitboss.Trace.Types.Identifier

mkTableShoeEntity :: Meta (EntityRef TableShoeEntityId) -> TableShoeEntityAttrs -> TableShoeEntityModes -> TableShoeEntityRels -> TableShoeEntity
mkTableShoeEntity = TableShoeEntity

mkTableShoeEntityAttrs :: [Card] -> TableShoeEntityAttrs
mkTableShoeEntityAttrs = TableShoeEntityAttrs

mkTableShoeEntityModes :: TableShoeEntityModes
mkTableShoeEntityModes = TableShoeEntityModes

mkTableShoeEntityRels :: EntityRef TableEntityId -> TableShoeEntityRels
mkTableShoeEntityRels = TableShoeEntityRels

data TableShoeEntity = TableShoeEntity
  { _shoeEntityMeta :: Meta (EntityRef TableShoeEntityId),
    _shoeEntityAttrs :: TableShoeEntityAttrs,
    _shoeEntityModes :: TableShoeEntityModes,
    _shoeEntityRels :: TableShoeEntityRels
  }
  deriving (Eq, Show, Generic)

data TableShoeEntityAttrs = TableShoeEntityAttrs
  { _shoeEntityAttrsCards :: [Card]
  }
  deriving (Eq, Show, Generic)

data TableShoeEntityModes = TableShoeEntityModes
  deriving (Eq, Show, Generic)

data TableShoeEntityRels = TableShoeEntityRels
  { _shoeEntityRelsTable :: EntityRef TableEntityId
  }
  deriving (Eq, Show, Generic)

instance ToJSON TableShoeEntity

instance FromJSON TableShoeEntity

instance ToJSON TableShoeEntityAttrs

instance FromJSON TableShoeEntityAttrs

instance ToJSON TableShoeEntityModes

instance FromJSON TableShoeEntityModes

instance ToJSON TableShoeEntityRels

instance FromJSON TableShoeEntityRels

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Pitboss.Trace.Entity.TableShoe.Types where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Card
import Pitboss.Trace.Types.EntityRef
import Pitboss.Trace.Types.Identifier

mkTableShoeEntityAttrs :: [Card] -> TableShoeEntityAttrs
mkTableShoeEntityAttrs = TableShoeEntityAttrs

mkTableShoeEntityModes :: TableShoeEntityModes
mkTableShoeEntityModes = TableShoeEntityModes

mkTableShoeEntityRels :: EntityRef TableEntityId -> TableShoeEntityRels
mkTableShoeEntityRels = TableShoeEntityRels

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

instance ToJSON TableShoeEntityAttrs

instance FromJSON TableShoeEntityAttrs

instance ToJSON TableShoeEntityModes

instance FromJSON TableShoeEntityModes

instance ToJSON TableShoeEntityRels

instance FromJSON TableShoeEntityRels

{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Pitboss.Trace.Entity.Table where

import Data.Aeson
import GHC.Generics hiding (Meta)
import Pitboss.Blackjack.Chips
import Pitboss.Trace.Entity.Types.Meta
import Pitboss.Trace.Types.EntityRef
import Pitboss.Trace.Types.Identifier

mkTableEntity :: Meta TableEntityId -> TableEntityAttrs -> TableEntityModes -> TableEntityRels -> TableEntity
mkTableEntity = TableEntity

mkTableEntityAttrs :: String -> Maybe (EntityRef DealerRoundEntityId) -> EntityRef OfferingEntityId -> Chips -> TableEntityAttrs
mkTableEntityAttrs = TableEntityAttrs

mkTableEntityModes :: Maybe (EntityRef DealerEntityId) -> TableEntityModes
mkTableEntityModes = TableEntityModes

mkTableEntityRels :: Maybe (EntityRef DealerEntityId) -> TableEntityRels
mkTableEntityRels = TableEntityRels

data TableEntity = TableEntity
    { _tableEntityMeta :: Meta TableEntityId
    , _tableEntityAttrs :: TableEntityAttrs
    , _tableEntityModes :: TableEntityModes
    , _tableEntityRels :: TableEntityRels
    }
    deriving (Eq, Show, Generic)

data TableEntityAttrs = TableEntityAttrs
    { _tableEntityAttrsName :: String
    , _tableEntityAttrsCurrentRound :: Maybe (EntityRef DealerRoundEntityId)
    , _tableEntityAttrsOfferingUsed :: EntityRef OfferingEntityId
    , _tableEntityAttrsMinBet :: Chips
    }
    deriving (Eq, Show, Generic)

data TableEntityModes = TableEntityModes
    { _tableEntityModesManagedByDealer :: Maybe (EntityRef DealerEntityId)
    }
    deriving (Eq, Show, Generic)

data TableEntityRels = TableEntityRels
    { _tableEntityRelsManagedByDealer :: Maybe (EntityRef DealerEntityId)
    }
    deriving (Eq, Show, Generic)

instance ToJSON TableEntity

instance FromJSON TableEntity

instance ToJSON TableEntityModes

instance FromJSON TableEntityModes

instance ToJSON TableEntityAttrs

instance FromJSON TableEntityAttrs

instance ToJSON TableEntityRels

instance FromJSON TableEntityRels

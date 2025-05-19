{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Pitboss.Trace.Entity.Table.Entity where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Chips
import Pitboss.FSM.Table
import Pitboss.Trace.Entity.Types.EntityId

mkTableEntityAttrs :: String -> Maybe (ClockedRef DealerRoundEntityId) -> ClockedRef OfferingEntityId -> Chips -> TableEntityAttrs
mkTableEntityAttrs = TableEntityAttrs

mkTableEntityModes :: SomeTableFSM -> TableEntityModes
mkTableEntityModes = TableEntityModes

mkTableEntityRels :: Maybe (ClockedRef DealerEntityId) -> TableEntityRels
mkTableEntityRels = TableEntityRels

data TableEntityAttrs = TableEntityAttrs
    { _tableEntityAttrsName :: String
    , _tableEntityAttrsCurrentRound :: Maybe (ClockedRef DealerRoundEntityId)
    , _tableEntityAttrsOfferingUsed :: ClockedRef OfferingEntityId
    , _tableEntityAttrsMinBet :: Chips
    }
    deriving (Eq, Show, Generic)

data TableEntityModes = TableEntityModes
    { _tableEntityModesFSM :: SomeTableFSM
    }
    deriving (Eq, Show, Generic)

data TableEntityRels = TableEntityRels
    { _tableEntityRelsManagedByDealer :: Maybe (ClockedRef DealerEntityId)
    }
    deriving (Eq, Show, Generic)

instance ToJSON TableEntityModes

instance FromJSON TableEntityModes

instance ToJSON TableEntityAttrs

instance FromJSON TableEntityAttrs

instance ToJSON TableEntityRels

instance FromJSON TableEntityRels

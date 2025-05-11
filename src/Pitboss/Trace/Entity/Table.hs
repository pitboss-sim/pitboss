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

mkTable :: Pitboss.Trace.Entity.Types.Meta.Meta TableId -> TableState -> TableRelations -> Table
mkTable = Table

mkTableState :: String -> Maybe (EntityRef DealerRoundId) -> EntityRef OfferingId -> Chips -> TableState
mkTableState = TableState

mkTableRelations :: Maybe (EntityRef DealerId) -> TableRelations
mkTableRelations = TableRelations

data Table = Table
  { _tableMeta :: Meta TableId,
    _tableState :: TableState,
    _tableRels :: TableRelations
  }
  deriving (Eq, Show, Generic)

data TableState = TableState
  { _tableStateName :: String,
    _tableStateCurrentRound :: Maybe (EntityRef DealerRoundId),
    _tableStateOfferingUsed :: EntityRef OfferingId,
    _tableStateMinBet :: Chips
  }
  deriving (Eq, Show, Generic)

data TableRelations = TableRelations
  { _tableRelsManagedByDealer :: Maybe (EntityRef DealerId)
  }
  deriving (Eq, Show, Generic)

instance ToJSON Table

instance FromJSON Table

instance ToJSON TableState

instance FromJSON TableState

instance ToJSON TableRelations

instance FromJSON TableRelations

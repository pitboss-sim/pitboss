{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Pitboss.Trace.Entity.Dealer where

import Data.Aeson.Types
import GHC.Generics (Generic)
import Pitboss.FSM.DealerHand
import Pitboss.FSM.DealerRound hiding (Dealer)
import Pitboss.FSM.DealerTable
import Pitboss.Trace.Entity.Types.Meta
import Pitboss.Trace.Types.EntityRef
import Pitboss.Trace.Types.Identifier

mkDealer :: Meta (EntityRef DealerId) -> DealerState -> SomeDealerTableFSM -> DealerRoundFSM -> SomeDealerHandFSM -> DealerRelations -> Dealer
mkDealer = Dealer

mkDealerState :: String -> Maybe (EntityRef TableId) -> DealerState
mkDealerState = DealerState

mkDealerRelations :: Maybe (EntityRef DealerRoundId) -> Maybe (EntityRef DealerHandId) -> DealerRelations
mkDealerRelations = DealerRelations

data Dealer = Dealer
  { _dealerMeta :: Meta (EntityRef DealerId),
    _dealerState :: DealerState,
    _dealerFsmTable :: SomeDealerTableFSM,
    _dealerFsmRound :: DealerRoundFSM,
    _dealerFsmHand :: SomeDealerHandFSM,
    _dealerRels :: DealerRelations
  }
  deriving (Eq, Show, Generic)

data DealerState = DealerState
  { _dealerStateName :: String,
    _dealerStateAssignedTable :: Maybe (EntityRef TableId)
  }
  deriving (Eq, Show, Generic)

data DealerRelations = DealerRelations
  { _dealerRelsCurrentRound :: Maybe (EntityRef DealerRoundId),
    _dealerRelsActiveHand :: Maybe (EntityRef DealerHandId)
  }
  deriving (Eq, Show, Generic)

instance ToJSON Dealer

instance FromJSON Dealer

instance ToJSON DealerState

instance FromJSON DealerState

instance ToJSON DealerRelations

instance FromJSON DealerRelations

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Pitboss.Trace.Entity.Dealer where

import Data.Aeson.Types
import GHC.Generics (Generic)
import Pitboss.FSM.DealerHandFSM
import Pitboss.FSM.DealerRoundFSM hiding (Dealer)
import Pitboss.FSM.DealerTableFSM
import Pitboss.Trace.Entity.Types.Meta
import Pitboss.Trace.Types.EntityRef
import Pitboss.Trace.Types.Identifier

mkDealer :: Meta DealerId -> DealerState -> SomeDealerTableFSM -> DealerRoundFSM -> SomeDealerHandFSM -> DealerRelations -> Dealer
mkDealer = Dealer

mkDealerState :: String -> Maybe (EntityRef TableId) -> DealerState
mkDealerState = DealerState

mkDealerRelations :: Maybe DealerRoundId -> Maybe DealerHandId -> DealerRelations
mkDealerRelations = DealerRelations

data Dealer = Dealer
  { _meta :: Meta DealerId,
    _state :: DealerState,
    _fsmTable :: SomeDealerTableFSM,
    _fsmRound :: DealerRoundFSM,
    _fsmHand :: SomeDealerHandFSM,
    _rels :: DealerRelations
  }
  deriving (Eq, Show, Generic)

data DealerState = DealerState
  { _dealerName :: String,
    _assignedTable :: Maybe (EntityRef TableId)
  }
  deriving (Eq, Show, Generic)

data DealerRelations = DealerRelations
  { _currentRound :: Maybe DealerRoundId,
    _activeHand :: Maybe DealerHandId
  }
  deriving (Eq, Show, Generic)

instance ToJSON Dealer

instance FromJSON Dealer

instance ToJSON DealerState

instance FromJSON DealerState

instance ToJSON DealerRelations

instance FromJSON DealerRelations

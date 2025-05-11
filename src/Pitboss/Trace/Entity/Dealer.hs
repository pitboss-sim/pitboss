{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Pitboss.Trace.Entity.Dealer where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.FSM.DealerHandFSM
import Pitboss.Trace.Entity.Table hiding (_tick)
import Pitboss.Trace.Timeline.EntityRef
import Pitboss.Trace.Timeline.Identifier
import Pitboss.Trace.Entity.Capabilities.Clocked
import Pitboss.FSM.DealerTableFSM

data DealerEntity = DealerEntity
  { _tick :: Tick,
    _state :: DealerState,
    _fsm :: SomeDealerTableFSM,
    _rels :: DealerRelations
  }
  deriving (Generic)

instance Show DealerEntity where
  show (DealerEntity t s f r) =
    "DealerEntity { _tick = "
      ++ show t
      ++ ", _state = "
      ++ show s
      ++ ", _fsm = "
      ++ show f
      ++ ", _rels = "
      ++ show r
      ++ " }"

instance Clocked DealerEntity where
  tick = _tick
  setTick t d = d {_tick = t}

instance ToJSON DealerEntity

instance FromJSON DealerEntity

data DealerState = DealerState
  { _dealerName :: String,
    _assignedTable :: Maybe (EntityRef TableState)
  }
  deriving (Eq, Show, Generic)

instance ToJSON DealerState

instance FromJSON DealerState

data DealerDelta
  = DealerStateDelta DealerStateChange
  | DealerRelationDelta DealerRelationChange
  deriving (Eq, Show, Generic)

instance ToJSON DealerDelta

instance FromJSON DealerDelta

data DealerStateChange
  = RenameDealer String
  | SetDealerTableFSM SomeDealerHandFSM
  | SetDealerRoundFSM SomeDealerHandFSM
  | SetDealerHandFSM SomeDealerHandFSM
  deriving (Eq, Show, Generic)

instance ToJSON DealerStateChange

instance FromJSON DealerStateChange

data DealerRelationChange
  = AssignTable TableId
  | UnassignTable
  | AssignRound RoundId
  | UnassignRound
  | AssignHand DealerHandId
  | UnassignHand
  deriving (Eq, Show, Generic)

instance ToJSON DealerRelationChange

instance FromJSON DealerRelationChange

data DealerRelations = DealerRelations
  { _currentRound :: Maybe RoundId,
    _activeHand :: Maybe DealerHandId
  }
  deriving (Eq, Show, Generic)

instance ToJSON DealerRelations

instance FromJSON DealerRelations

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Pitboss.Trace.Entity.Dealer where

import Data.Aeson.Types
import Data.Text
import Data.Text qualified as T
import GHC.Generics (Generic)
import Pitboss.FSM.DealerHandFSM
import Pitboss.FSM.DealerRoundFSM
import Pitboss.FSM.DealerTableFSM
import Pitboss.Trace.Entity.Capabilities.Clocked
import Pitboss.Trace.Entity.Capabilities.Reversible
import Pitboss.Trace.Entity.Table hiding (_currentRound, _rels, _state, _tick)
import Pitboss.Trace.EntityRegistry.EntityRef
import Pitboss.Trace.EntityRegistry.Identifier

mkDealerEntity ::
  Tick ->
  DealerState ->
  SomeDealerTableFSM ->
  DealerRoundFSM ->
  SomeDealerHandFSM ->
  DealerRelations ->
  DealerEntity
mkDealerEntity t state fsmTable fsmRound fsmHand rels =
  DealerEntity
    { _tick = t,
      _state = state,
      _fsmTable = fsmTable,
      _fsmRound = fsmRound,
      _fsmHand = fsmHand,
      _rels = rels
    }

data DealerEntity = DealerEntity
  { _tick :: Tick,
    _state :: DealerState,
    _fsmTable :: SomeDealerTableFSM,
    _fsmRound :: DealerRoundFSM,
    _fsmHand :: SomeDealerHandFSM,
    _rels :: DealerRelations
  }
  deriving (Eq, Show, Generic)

data DealerState = DealerState
  { _dealerName :: String,
    _assignedTable :: Maybe (EntityRef TableEntity)
  }
  deriving (Eq, Show, Generic)

data DealerRelations = DealerRelations
  { _currentRound :: Maybe RoundId,
    _activeHand :: Maybe DealerHandId
  }
  deriving (Eq, Show, Generic)

instance Clocked DealerEntity where
  tick = _tick
  setTick t d = d {_tick = t}

data DealerStateDelta
  = RenameDealer String String
  | ReplaceAssignedTable (Maybe (EntityRef TableEntity)) (Maybe (EntityRef TableEntity))
  deriving (Eq, Show, Generic)

data DealerRelationsDelta
  = UpdateRound (Maybe RoundId) (Maybe RoundId)
  | UpdateHand (Maybe DealerHandId) (Maybe DealerHandId)
  deriving (Eq, Show, Generic)

data DealerFSMDelta
  = ReplaceTableFSM SomeDealerTableFSM SomeDealerTableFSM
  | ReplaceRoundFSM DealerRoundFSM DealerRoundFSM
  | ReplaceHandFSM SomeDealerHandFSM SomeDealerHandFSM
  deriving (Eq, Show, Generic)

data DealerEntityDelta
  = DealerStateDelta DealerStateDelta
  | DealerRelationsDelta DealerRelationsDelta
  | DealerFSMDelta DealerFSMDelta
  deriving (Eq, Show, Generic)

instance ToJSON DealerEntity

instance FromJSON DealerEntity

instance ToJSON DealerState

instance FromJSON DealerState

instance ToJSON DealerRelations

instance FromJSON DealerRelations

instance ToJSON DealerEntityDelta

instance FromJSON DealerEntityDelta

instance ToJSON DealerStateDelta

instance FromJSON DealerStateDelta

instance ToJSON DealerRelationsDelta

instance FromJSON DealerRelationsDelta

instance ToJSON DealerFSMDelta where
  toJSON = \case
    ReplaceTableFSM _ new ->
      object ["tag" .= String "ReplaceTableFSM", "new" .= new]
    ReplaceRoundFSM _ new ->
      object ["tag" .= String "ReplaceRoundFSM", "new" .= new]
    ReplaceHandFSM _ new ->
      object ["tag" .= String "ReplaceHandFSM", "new" .= new]

instance FromJSON DealerFSMDelta where
  parseJSON = withObject "DealerFSMDelta" $ \obj -> do
    tag <- obj .: "tag"
    case tag :: Text of
      "ReplaceTableFSM" ->
        ReplaceTableFSM undefined <$> obj .: "new"
      "ReplaceRoundFSM" ->
        ReplaceRoundFSM undefined <$> obj .: "new"
      "ReplaceHandFSM" ->
        ReplaceHandFSM undefined <$> obj .: "new"
      _ -> fail $ "Unknown tag for DealerFSMDelta: " ++ T.unpack tag

instance Reversible DealerStateDelta where
  invert = \case
    RenameDealer old new -> Right (RenameDealer new old)
    ReplaceAssignedTable old new -> Right (ReplaceAssignedTable new old)

instance Reversible DealerRelationsDelta where
  invert = \case
    UpdateRound old new -> Right (UpdateRound new old)
    UpdateHand old new -> Right (UpdateHand new old)

instance Reversible DealerFSMDelta where
  invert = \case
    ReplaceTableFSM old new -> Right (ReplaceTableFSM new old)
    ReplaceRoundFSM old new -> Right (ReplaceRoundFSM new old)
    ReplaceHandFSM old new -> Right (ReplaceHandFSM new old)

instance Reversible DealerEntityDelta where
  invert = \case
    DealerStateDelta d -> DealerStateDelta <$> invert d
    DealerRelationsDelta d -> DealerRelationsDelta <$> invert d
    DealerFSMDelta d -> DealerFSMDelta <$> invert d

applyDealerStateDelta :: DealerStateDelta -> DealerState -> DealerState
applyDealerStateDelta delta state = case delta of
  RenameDealer _ new -> state {_dealerName = new}
  ReplaceAssignedTable _ new -> state {_assignedTable = new}

applyDealerRelationsDelta :: DealerRelationsDelta -> DealerRelations -> DealerRelations
applyDealerRelationsDelta delta rels = case delta of
  UpdateRound _ new -> rels {_currentRound = new}
  UpdateHand _ new -> rels {_activeHand = new}

applyDealerFSMDelta ::
  DealerFSMDelta ->
  (SomeDealerTableFSM, DealerRoundFSM, SomeDealerHandFSM) ->
  (SomeDealerTableFSM, DealerRoundFSM, SomeDealerHandFSM)
applyDealerFSMDelta delta (ft, fr, fh) = case delta of
  ReplaceTableFSM _ new -> (new, fr, fh)
  ReplaceRoundFSM _ new -> (ft, new, fh)
  ReplaceHandFSM _ new -> (ft, fr, new)

applyDealerEntityDelta :: DealerEntityDelta -> DealerEntity -> DealerEntity
applyDealerEntityDelta delta entity = case delta of
  DealerStateDelta d ->
    entity {_state = applyDealerStateDelta d (_state entity)}
  DealerRelationsDelta d ->
    entity {_rels = applyDealerRelationsDelta d (_rels entity)}
  DealerFSMDelta d ->
    let (ft, fr, fh) = applyDealerFSMDelta d (_fsmTable entity, _fsmRound entity, _fsmHand entity)
     in entity {_fsmTable = ft, _fsmRound = fr, _fsmHand = fh}

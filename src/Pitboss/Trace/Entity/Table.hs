{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Pitboss.Trace.Entity.Table where

import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe
import GHC.Generics (Generic)
import Pitboss.Blackjack.Chips (Chips)
import Pitboss.Trace.Entity.Capabilities.Clocked
import Pitboss.Trace.Entity.Capabilities.Reversible
import Pitboss.Trace.Entity.DealerRound hiding (_rels, _state, _tick)
import Pitboss.Trace.Entity.Offering hiding (_rels, _state, _tick)
import Pitboss.Trace.EntityRegistry.EntityRef
import Pitboss.Trace.EntityRegistry.Identifier

mkTableEntity ::
  Tick ->
  TableState ->
  TableRelations ->
  TableEntity
mkTableEntity t s rels =
  TableEntity
    { _tick = t,
      _state = s,
      _rels = rels
    }

mkTableState ::
  String ->
  EntityRef OfferingState ->
  Chips ->
  TableState
mkTableState name offering minBet =
  TableState
    { _tableName = name,
      _offeringUsed = offering,
      _minBet = minBet,
      _currentRound = Nothing
    }

data TableEntity = TableEntity
  { _tick :: Tick,
    _state :: TableState,
    _rels :: TableRelations
  }
  deriving (Eq, Show, Generic)

data TableState = TableState
  { _tableName :: String,
    _currentRound :: Maybe (EntityRef DealerRoundState),
    _offeringUsed :: EntityRef OfferingState,
    _minBet :: Chips
  }
  deriving (Eq, Show, Generic)

data TableRelations = TableRelations
  { _managedByDealer :: Maybe DealerId
  }
  deriving (Eq, Show, Generic)

data TableStateDelta
  = SetTableName String String
  | SetMinBet Chips Chips
  | SetOffering (EntityRef OfferingState) (EntityRef OfferingState)
  | StartRound (Maybe (EntityRef DealerRoundState)) (EntityRef DealerRoundState)
  | EndRound (EntityRef DealerRoundState)
  deriving (Eq, Show, Generic)

data TableRelationsDelta
  = AssignDealer (Maybe DealerId) DealerId
  | UnassignDealer DealerId
  deriving (Eq, Show, Generic)

data TableEntityDelta
  = TableStateDelta TableStateDelta
  | TableRelationsDelta TableRelationsDelta
  deriving (Eq, Show, Generic)

instance ToJSON TableEntity

instance FromJSON TableEntity

instance ToJSON TableState

instance FromJSON TableState

instance ToJSON TableStateDelta

instance FromJSON TableStateDelta

instance ToJSON TableRelations

instance FromJSON TableRelations

instance ToJSON TableRelationsDelta

instance FromJSON TableRelationsDelta

instance ToJSON TableEntityDelta

instance FromJSON TableEntityDelta

instance Clocked TableEntity where
  tick = _tick
  setTick t s = s {_tick = t}

instance Reversible TableStateDelta where
  invert = \case
    SetTableName from to -> Right (SetTableName to from)
    SetMinBet from to -> Right (SetMinBet to from)
    SetOffering from to -> Right (SetOffering to from)
    StartRound _ new -> Right (EndRound new)
    EndRound old -> Right (StartRound (Just old) old)

instance Reversible TableRelationsDelta where
  invert = \case
    AssignDealer prev new -> Right (AssignDealer (Just new) (fromMaybe undefined prev))
    UnassignDealer old -> Right (AssignDealer (Just old) old)

instance Reversible TableEntityDelta where
  invert = \case
    TableStateDelta d -> TableStateDelta <$> invert d
    TableRelationsDelta d -> TableRelationsDelta <$> invert d

applyTableStateDelta :: TableStateDelta -> TableState -> TableState
applyTableStateDelta delta s = case delta of
  SetTableName _ new -> s {_tableName = new}
  SetMinBet _ new -> s {_minBet = new}
  SetOffering _ new -> s {_offeringUsed = new}
  StartRound _ new -> s {_currentRound = Just new}
  EndRound _ -> s {_currentRound = Nothing}

applyTableRelationsDelta :: TableRelationsDelta -> TableRelations -> TableRelations
applyTableRelationsDelta delta rels = case delta of
  AssignDealer _ new -> rels {_managedByDealer = Just new}
  UnassignDealer _ -> rels {_managedByDealer = Nothing}

applyTableEntityDelta :: TableEntityDelta -> TableEntity -> TableEntity
applyTableEntityDelta delta entity = case delta of
  TableStateDelta d -> entity {_state = applyTableStateDelta d (_state entity)}
  TableRelationsDelta d -> entity {_rels = applyTableRelationsDelta d (_rels entity)}

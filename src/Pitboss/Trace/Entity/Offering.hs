{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Pitboss.Trace.Entity.Offering where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Offering (Offering)
import Pitboss.Trace.Entity.Capabilities.Clocked
import Pitboss.Trace.Entity.Capabilities.Reversible
import Pitboss.Trace.EntityRegistry.Identifier

data OfferingEntity = OfferingEntity
  { _tick :: Tick,
    _state :: OfferingState,
    _rels :: OfferingRelations
  }
  deriving (Eq, Show, Generic)

data OfferingState = OfferingState
  { _offering :: Offering
  }
  deriving (Eq, Show, Generic)

data OfferingRelations = OfferingRelations
  { _associatedTables :: [TableId]
  }
  deriving (Eq, Show, Generic)

data OfferingStateDelta
  = ReplaceOffering Offering Offering
  deriving (Eq, Show, Generic)

data OfferingRelationsDelta
  = AddTable TableId
  | RemoveTable TableId
  deriving (Eq, Show, Generic)

data OfferingEntityDelta
  = OfferingStateDelta OfferingStateDelta
  | OfferingRelationsDelta OfferingRelationsDelta
  deriving (Eq, Show, Generic)

mkOfferingState :: Offering -> OfferingState
mkOfferingState = OfferingState

mkOfferingEntity :: Tick -> OfferingState -> OfferingRelations -> OfferingEntity
mkOfferingEntity = OfferingEntity

mkOfferingRelations :: OfferingRelations
mkOfferingRelations = OfferingRelations []

instance Clocked OfferingEntity where
  tick = _tick
  setTick t e = e {_tick = t}

instance Reversible OfferingStateDelta where
  invert = \case
    ReplaceOffering from to -> Right (ReplaceOffering to from)

instance Reversible OfferingRelationsDelta where
  invert = \case
    AddTable tid -> Right (RemoveTable tid)
    RemoveTable tid -> Right (AddTable tid)

instance Reversible OfferingEntityDelta where
  invert = \case
    OfferingStateDelta d -> OfferingStateDelta <$> invert d
    OfferingRelationsDelta d -> OfferingRelationsDelta <$> invert d

applyOfferingStateDelta :: OfferingStateDelta -> OfferingState -> OfferingState
applyOfferingStateDelta delta s = case delta of
  ReplaceOffering _ new -> s {_offering = new}

applyOfferingRelationsDelta :: OfferingRelationsDelta -> OfferingRelations -> OfferingRelations
applyOfferingRelationsDelta delta rels = case delta of
  AddTable tid -> rels {_associatedTables = tid : _associatedTables rels}
  RemoveTable tid -> rels {_associatedTables = filter (/= tid) (_associatedTables rels)}

applyOfferingEntityDelta :: OfferingEntityDelta -> OfferingEntity -> OfferingEntity
applyOfferingEntityDelta delta entity = case delta of
  OfferingStateDelta d -> entity {_state = applyOfferingStateDelta d (_state entity)}
  OfferingRelationsDelta d -> entity {_rels = applyOfferingRelationsDelta d (_rels entity)}

instance ToJSON OfferingEntity

instance FromJSON OfferingEntity

instance ToJSON OfferingState

instance FromJSON OfferingState

instance ToJSON OfferingRelations

instance FromJSON OfferingRelations

instance ToJSON OfferingStateDelta

instance FromJSON OfferingStateDelta

instance ToJSON OfferingRelationsDelta

instance FromJSON OfferingRelationsDelta

instance ToJSON OfferingEntityDelta

instance FromJSON OfferingEntityDelta

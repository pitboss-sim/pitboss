{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Pitboss.Trace.Entity.ShoeCursor where

import GHC.Generics (Generic)
import Pitboss.Trace.Entity.Capabilities.Clocked
import Pitboss.Trace.Entity.Capabilities.Reversible
import Pitboss.Trace.EntityRegistry.Identifier

data ShoeCursorEntity = ShoeCursorEntity
  { _tick :: Tick,
    _offset :: Int,
    _rels :: ShoeCursorEntityRelations
  }
  deriving (Eq, Show, Generic)

data ShoeCursorEntityRelations = ShoeCursorEntityRelations
  { _pointsToShoe :: ShoeId
  }
  deriving (Eq, Show, Generic)

data ShoeCursorStateDelta
  = Advance Int
  | Rewind Int
  | ReplaceOffset Int Int
  deriving (Eq, Show, Generic)

data ShoeCursorRelationsDelta
  = UpdateShoe ShoeId ShoeId
  deriving (Eq, Show, Generic)

data ShoeCursorEntityDelta
  = ShoeCursorStateDelta ShoeCursorStateDelta
  | ShoeCursorRelationsDelta ShoeCursorRelationsDelta
  deriving (Eq, Show, Generic)

mkShoeCursorEntity ::
  Tick ->
  Int ->
  ShoeCursorEntityRelations ->
  ShoeCursorEntity
mkShoeCursorEntity = ShoeCursorEntity

instance Clocked ShoeCursorEntity where
  tick = _tick
  setTick t e = e {_tick = t}

instance Reversible ShoeCursorStateDelta where
  invert = \case
    Advance n -> Right (Rewind n)
    Rewind n -> Right (Advance n)
    ReplaceOffset old new -> Right (ReplaceOffset new old)

instance Reversible ShoeCursorRelationsDelta where
  invert = \case
    UpdateShoe old new -> Right (UpdateShoe new old)

instance Reversible ShoeCursorEntityDelta where
  invert = \case
    ShoeCursorStateDelta d -> ShoeCursorStateDelta <$> invert d
    ShoeCursorRelationsDelta d -> ShoeCursorRelationsDelta <$> invert d

applyShoeCursorStateDelta :: ShoeCursorStateDelta -> ShoeCursorEntity -> ShoeCursorEntity
applyShoeCursorStateDelta delta entity = case delta of
  Advance n -> entity {_offset = _offset entity + n}
  Rewind n -> entity {_offset = _offset entity - n}
  ReplaceOffset _ new -> entity {_offset = new}

applyShoeCursorRelationsDelta :: ShoeCursorRelationsDelta -> ShoeCursorEntityRelations -> ShoeCursorEntityRelations
applyShoeCursorRelationsDelta delta rels = case delta of
  UpdateShoe _ new -> rels {_pointsToShoe = new}

applyShoeCursorEntityDelta :: ShoeCursorEntityDelta -> ShoeCursorEntity -> ShoeCursorEntity
applyShoeCursorEntityDelta delta entity = case delta of
  ShoeCursorStateDelta d ->
    entity {_offset = _offset (applyShoeCursorStateDelta d entity)}
  ShoeCursorRelationsDelta d ->
    entity {_rels = applyShoeCursorRelationsDelta d (_rels entity)}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.PlayerSpot where

import Data.Aeson
import GHC.Generics (Generic)
import Pitboss.Blackjack.Chips (Chips)
import Pitboss.FSM.PlayerSpotFSM
import Pitboss.Trace.Entity.Capabilities.Clocked
import Pitboss.Trace.Entity.Capabilities.Reversible
import Pitboss.Trace.Entity.Types.FiniteMap
import Pitboss.Trace.Entity.Types.FiniteMap.BoundedEnum
import Pitboss.Trace.Entity.Types.FiniteMap.Occupancy
import Pitboss.Trace.EntityRegistry.Identifier

data PlayerSpotIx = PlayerSpot1 | PlayerSpot2 | PlayerSpot3 | PlayerSpot4
  deriving (Eq, Show, Ord, Enum, Bounded, Generic)

instance ToJSONKey PlayerSpotIx

instance FromJSONKey PlayerSpotIx

instance ToJSON PlayerSpotIx

instance FromJSON PlayerSpotIx

instance BoundedEnum PlayerSpotIx

data PlayerSpotHandIx = PlayerSpotHand1 | PlayerSpotHand2 | PlayerSpotHand3 | PlayerSpotHand4
  deriving (Eq, Show, Ord, Enum, Bounded, Generic)

instance ToJSONKey PlayerSpotHandIx

instance FromJSONKey PlayerSpotHandIx

instance ToJSON PlayerSpotHandIx

instance FromJSON PlayerSpotHandIx

instance BoundedEnum PlayerSpotHandIx

data PlayerSpotEntity = PlayerSpotEntity
  { _tick :: Tick,
    _fsm :: SomePlayerSpotFSM,
    _state :: PlayerSpotState,
    _rels :: PlayerSpotEntityRelations
  }
  deriving (Eq, Show, Generic)

data PlayerSpotState = PlayerSpotState
  { _spotIndex :: PlayerSpotIx,
    _wager :: Chips,
    _handOccupancy :: FiniteMap PlayerSpotHandIx (Occupancy PlayerHandId)
  }
  deriving (Eq, Show, Generic)

data PlayerSpotEntityRelations = PlayerSpotEntityRelations
  { _playerId :: PlayerId,
    _roundId :: RoundId
  }
  deriving (Eq, Show, Generic)

data PlayerSpotStateDelta
  = ReplaceWager Chips Chips
  | UpdateHandOccupancy (PlayerSpotHandIx, Occupancy PlayerHandId) (PlayerSpotHandIx, Occupancy PlayerHandId)
  deriving (Eq, Show, Generic)

data PlayerSpotRelationsDelta
  = UpdatePlayer PlayerId PlayerId
  | UpdateRound RoundId RoundId
  deriving (Eq, Show, Generic)

data PlayerSpotFSMDelta
  = ReplaceFSM SomePlayerSpotFSM SomePlayerSpotFSM
  deriving (Eq, Show, Generic)

data PlayerSpotEntityDelta
  = PlayerSpotStateDelta PlayerSpotStateDelta
  | PlayerSpotRelationsDelta PlayerSpotRelationsDelta
  | PlayerSpotFSMDelta PlayerSpotFSMDelta
  deriving (Eq, Show, Generic)

mkPlayerSpotEntity ::
  Tick ->
  SomePlayerSpotFSM ->
  PlayerSpotState ->
  PlayerSpotEntityRelations ->
  PlayerSpotEntity
mkPlayerSpotEntity t fsm state rels =
  PlayerSpotEntity
    { _tick = t,
      _fsm = fsm,
      _state = state,
      _rels = rels
    }

mkPlayerSpotState ::
  PlayerSpotIx ->
  Chips ->
  PlayerSpotState
mkPlayerSpotState ix wager =
  PlayerSpotState
    { _spotIndex = ix,
      _wager = wager,
      _handOccupancy = mempty
    }

instance Clocked PlayerSpotEntity where
  tick = _tick
  setTick t e = e {_tick = t}

instance Reversible PlayerSpotStateDelta where
  invert = \case
    ReplaceWager old new -> Right (ReplaceWager new old)
    UpdateHandOccupancy old new -> Right (UpdateHandOccupancy new old)

instance Reversible PlayerSpotRelationsDelta where
  invert = \case
    UpdatePlayer old new -> Right (UpdatePlayer new old)
    UpdateRound old new -> Right (UpdateRound new old)

instance Reversible PlayerSpotFSMDelta where
  invert = \case
    ReplaceFSM old new -> Right (ReplaceFSM new old)

instance Reversible PlayerSpotEntityDelta where
  invert = \case
    PlayerSpotStateDelta d -> PlayerSpotStateDelta <$> invert d
    PlayerSpotRelationsDelta d -> PlayerSpotRelationsDelta <$> invert d
    PlayerSpotFSMDelta d -> PlayerSpotFSMDelta <$> invert d

instance ToJSON PlayerSpotEntity

instance FromJSON PlayerSpotEntity

instance ToJSON PlayerSpotState

instance FromJSON PlayerSpotState

instance ToJSON PlayerSpotEntityRelations

instance FromJSON PlayerSpotEntityRelations

instance ToJSON PlayerSpotStateDelta

instance FromJSON PlayerSpotStateDelta

instance ToJSON PlayerSpotRelationsDelta

instance FromJSON PlayerSpotRelationsDelta

instance ToJSON PlayerSpotFSMDelta

instance FromJSON PlayerSpotFSMDelta

instance ToJSON PlayerSpotEntityDelta

instance FromJSON PlayerSpotEntityDelta

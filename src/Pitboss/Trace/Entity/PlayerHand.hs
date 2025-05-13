{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Pitboss.Trace.Entity.PlayerHand where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Card (Card)
import Pitboss.Blackjack.Chips (Chips)
import Pitboss.FSM.PlayerHandFSM
import Pitboss.Trace.Entity.Capabilities.Clocked
import Pitboss.Trace.Entity.Capabilities.Reversible
import Pitboss.Trace.EntityRegistry.Identifier

mkPlayerHandEntity ::
  Tick ->
  PlayerHandState ->
  SomePlayerHandFSM ->
  PlayerHandEntityRelations ->
  PlayerHandEntity
mkPlayerHandEntity t s fsm rels =
  PlayerHandEntity
    { _tick = t,
      _state = s,
      _fsm = fsm,
      _rels = rels
    }

mkPlayerHandState :: [Card] -> Chips -> Int -> Int -> PlayerHandState
mkPlayerHandState cards bet depth ix =
  PlayerHandState
    { _handCards = cards,
      _originalBet = bet,
      _splitDepth = depth,
      _handIx = ix
    }

data PlayerHandEntity = PlayerHandEntity
  { _tick :: Tick,
    _fsm :: SomePlayerHandFSM,
    _state :: PlayerHandState,
    _rels :: PlayerHandEntityRelations
  }
  deriving (Eq, Show, Generic)

data PlayerHandState = PlayerHandState
  { _handCards :: [Card],
    _originalBet :: Chips,
    _splitDepth :: Int,
    _handIx :: Int
  }
  deriving (Eq, Show, Generic)

data PlayerHandEntityRelations = PlayerHandEntityRelations
  { _belongsToPlayerSpot :: PlayerSpotId,
    _belongsToRound :: RoundId,
    _ownedByPlayer :: PlayerId
  }
  deriving (Eq, Show, Generic)

data PlayerHandStateDelta
  = AddCard Card
  | RemoveCard Card
  | ReplaceCards [Card] [Card]
  | ReplacePlayerHandIndex Int Int
  | ReplaceSplitDepth Int Int
  deriving (Eq, Show, Generic)

data PlayerHandRelationsDelta
  = UpdatePlayerSpot PlayerSpotId PlayerSpotId
  | UpdateRound RoundId RoundId
  | UpdatePlayer PlayerId PlayerId
  deriving (Eq, Show, Generic)

data PlayerHandFSMDelta
  = ReplaceFSM SomePlayerHandFSM SomePlayerHandFSM
  deriving (Eq, Show, Generic)

data PlayerHandEntityDelta
  = PlayerHandStateDelta PlayerHandStateDelta
  | PlayerHandRelationsDelta PlayerHandRelationsDelta
  | PlayerHandFSMDelta PlayerHandFSMDelta
  deriving (Eq, Show, Generic)

instance ToJSON PlayerHandEntity

instance FromJSON PlayerHandEntity

instance ToJSON PlayerHandState

instance FromJSON PlayerHandState

instance ToJSON PlayerHandEntityRelations

instance FromJSON PlayerHandEntityRelations

instance ToJSON PlayerHandStateDelta

instance FromJSON PlayerHandStateDelta

instance ToJSON PlayerHandRelationsDelta

instance FromJSON PlayerHandRelationsDelta

instance ToJSON PlayerHandFSMDelta

instance FromJSON PlayerHandFSMDelta

instance ToJSON PlayerHandEntityDelta

instance FromJSON PlayerHandEntityDelta

instance Clocked PlayerHandEntity where
  tick = _tick
  setTick t hs = hs {_tick = t}

instance Reversible PlayerHandStateDelta where
  invert = \case
    AddCard c -> Right (RemoveCard c)
    RemoveCard c -> Right (AddCard c)
    ReplaceCards old new -> Right (ReplaceCards new old)
    ReplacePlayerHandIndex old new -> Right (ReplacePlayerHandIndex new old)
    ReplaceSplitDepth old new -> Right (ReplaceSplitDepth new old)

instance Reversible PlayerHandRelationsDelta where
  invert = \case
    UpdatePlayerSpot old new -> Right (UpdatePlayerSpot new old)
    UpdateRound old new -> Right (UpdateRound new old)
    UpdatePlayer old new -> Right (UpdatePlayer new old)

instance Reversible PlayerHandFSMDelta where
  invert (ReplaceFSM old new) = Right (ReplaceFSM new old)

instance Reversible PlayerHandEntityDelta where
  invert = \case
    PlayerHandStateDelta d -> PlayerHandStateDelta <$> invert d
    PlayerHandRelationsDelta d -> PlayerHandRelationsDelta <$> invert d
    PlayerHandFSMDelta d -> PlayerHandFSMDelta <$> invert d

applyPlayerHandStateDelta :: PlayerHandStateDelta -> PlayerHandState -> PlayerHandState
applyPlayerHandStateDelta delta state = case delta of
  AddCard c -> state {_handCards = _handCards state ++ [c]}
  RemoveCard c -> state {_handCards = filter (/= c) (_handCards state)}
  ReplaceCards _ new -> state {_handCards = new}
  ReplacePlayerHandIndex _ new -> state {_handIx = new}
  ReplaceSplitDepth _ new -> state {_splitDepth = new}

applyPlayerHandRelationsDelta :: PlayerHandRelationsDelta -> PlayerHandEntityRelations -> PlayerHandEntityRelations
applyPlayerHandRelationsDelta delta rels = case delta of
  UpdatePlayerSpot _ new -> rels {_belongsToPlayerSpot = new}
  UpdateRound _ new -> rels {_belongsToRound = new}
  UpdatePlayer _ new -> rels {_ownedByPlayer = new}

applyPlayerHandFSMDelta :: PlayerHandFSMDelta -> SomePlayerHandFSM -> SomePlayerHandFSM
applyPlayerHandFSMDelta (ReplaceFSM _ new) _ = new

applyPlayerHandEntityDelta :: PlayerHandEntityDelta -> PlayerHandEntity -> PlayerHandEntity
applyPlayerHandEntityDelta delta entity = case delta of
  PlayerHandStateDelta d ->
    entity {_state = applyPlayerHandStateDelta d (_state entity)}
  PlayerHandRelationsDelta d ->
    entity {_rels = applyPlayerHandRelationsDelta d (_rels entity)}
  PlayerHandFSMDelta d ->
    entity {_fsm = applyPlayerHandFSMDelta d (_fsm entity)}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Pitboss.Trace.Entity.DealerHand where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Card (Card)
import Pitboss.Blackjack.Chips (Chips)
import Pitboss.FSM.DealerHandFSM
import Pitboss.Trace.Entity.Capabilities.Clocked
import Pitboss.Trace.Entity.Capabilities.Reversible
import Pitboss.Trace.EntityRegistry.Identifier

mkDealerHandEntity ::
  Tick ->
  DealerHandState ->
  SomeDealerHandFSM ->
  DealerHandEntityRelations ->
  DealerHandEntity
mkDealerHandEntity t s fsm rels =
  DealerHandEntity
    { _tick = t,
      _state = s,
      _fsm = fsm,
      _rels = rels
    }

mkDealerHandState :: [Card] -> Chips -> Int -> Int -> DealerHandState
mkDealerHandState cards bet depth ix =
  DealerHandState
    { _handCards = cards,
      _originalBet = bet,
      _splitDepth = depth,
      _handIx = ix
    }

data DealerHandEntity = DealerHandEntity
  { _tick :: Tick,
    _fsm :: SomeDealerHandFSM,
    _state :: DealerHandState,
    _rels :: DealerHandEntityRelations
  }
  deriving (Eq, Show, Generic)

data DealerHandState = DealerHandState
  { _handCards :: [Card],
    _originalBet :: Chips,
    _splitDepth :: Int,
    _handIx :: Int
  }
  deriving (Eq, Show, Generic)

data DealerHandEntityRelations = DealerHandEntityRelations
  { _belongsToPlayerSpot :: PlayerSpotId,
    _belongsToRound :: RoundId,
    _ownedByDealer :: DealerId
  }
  deriving (Eq, Show, Generic)

data DealerHandStateDelta
  = AddCard Card
  | RemoveCard Card
  | ReplaceCards [Card] [Card]
  | ReplaceDealerHandIndex Int Int
  | ReplaceSplitDepth Int Int
  deriving (Eq, Show, Generic)

data DealerHandRelationsDelta
  = UpdatePlayerSpot PlayerSpotId PlayerSpotId
  | UpdateRound RoundId RoundId
  | UpdateDealer DealerId DealerId
  deriving (Eq, Show, Generic)

data DealerHandFSMDelta
  = ReplaceFSM SomeDealerHandFSM SomeDealerHandFSM
  deriving (Eq, Show, Generic)

data DealerHandEntityDelta
  = DealerHandStateDelta DealerHandStateDelta
  | DealerHandRelationsDelta DealerHandRelationsDelta
  | DealerHandFSMDelta DealerHandFSMDelta
  deriving (Eq, Show, Generic)

instance ToJSON DealerHandEntity

instance FromJSON DealerHandEntity

instance ToJSON DealerHandState

instance FromJSON DealerHandState

instance ToJSON DealerHandEntityRelations

instance FromJSON DealerHandEntityRelations

instance ToJSON DealerHandStateDelta

instance FromJSON DealerHandStateDelta

instance ToJSON DealerHandRelationsDelta

instance FromJSON DealerHandRelationsDelta

instance ToJSON DealerHandFSMDelta

instance FromJSON DealerHandFSMDelta

instance ToJSON DealerHandEntityDelta

instance FromJSON DealerHandEntityDelta

instance Clocked DealerHandEntity where
  tick = _tick
  setTick t hs = hs {_tick = t}

instance Reversible DealerHandStateDelta where
  invert = \case
    AddCard c -> Right (RemoveCard c)
    RemoveCard c -> Right (AddCard c)
    ReplaceCards old new -> Right (ReplaceCards new old)
    ReplaceDealerHandIndex old new -> Right (ReplaceDealerHandIndex new old)
    ReplaceSplitDepth old new -> Right (ReplaceSplitDepth new old)

instance Reversible DealerHandRelationsDelta where
  invert = \case
    UpdatePlayerSpot old new -> Right (UpdatePlayerSpot new old)
    UpdateRound old new -> Right (UpdateRound new old)
    UpdateDealer old new -> Right (UpdateDealer new old)

instance Reversible DealerHandFSMDelta where
  invert (ReplaceFSM old new) = Right (ReplaceFSM new old)

instance Reversible DealerHandEntityDelta where
  invert = \case
    DealerHandStateDelta d -> DealerHandStateDelta <$> invert d
    DealerHandRelationsDelta d -> DealerHandRelationsDelta <$> invert d
    DealerHandFSMDelta d -> DealerHandFSMDelta <$> invert d

applyDealerHandStateDelta :: DealerHandStateDelta -> DealerHandState -> DealerHandState
applyDealerHandStateDelta delta state = case delta of
  AddCard c -> state {_handCards = c : _handCards state}
  RemoveCard c -> state {_handCards = filter (/= c) (_handCards state)}
  ReplaceCards _ new -> state {_handCards = new}
  ReplaceDealerHandIndex _ new -> state {_handIx = new}
  ReplaceSplitDepth _ new -> state {_splitDepth = new}

applyDealerHandRelationsDelta :: DealerHandRelationsDelta -> DealerHandEntityRelations -> DealerHandEntityRelations
applyDealerHandRelationsDelta delta rels = case delta of
  UpdatePlayerSpot _ new -> rels {_belongsToPlayerSpot = new}
  UpdateRound _ new -> rels {_belongsToRound = new}
  UpdateDealer _ new -> rels {_ownedByDealer = new}

applyDealerHandFSMDelta :: DealerHandFSMDelta -> SomeDealerHandFSM -> SomeDealerHandFSM
applyDealerHandFSMDelta (ReplaceFSM _ new) _ = new

applyDealerHandEntityDelta :: DealerHandEntityDelta -> DealerHandEntity -> DealerHandEntity
applyDealerHandEntityDelta delta entity = case delta of
  DealerHandStateDelta d ->
    entity {_state = applyDealerHandStateDelta d (_state entity)}
  DealerHandRelationsDelta d ->
    entity {_rels = applyDealerHandRelationsDelta d (_rels entity)}
  DealerHandFSMDelta d ->
    entity {_fsm = applyDealerHandFSMDelta d (_fsm entity)}

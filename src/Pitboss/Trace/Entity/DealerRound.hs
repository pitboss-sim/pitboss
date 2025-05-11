{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Pitboss.Trace.Entity.DealerRound where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Trace.Entity.Capabilities.Clocked
import Pitboss.Trace.Entity.Capabilities.Reversible
import Pitboss.Trace.Entity.Shoe hiding (_tick)
import Pitboss.Trace.EntityRegistry.EntityRef

mkDealerRoundState :: Int -> DealerRoundState
mkDealerRoundState n = DealerRoundState n False

mkDealerRoundEntity :: Tick -> DealerRoundState -> DealerRoundRelations -> DealerRoundEntity
mkDealerRoundEntity = DealerRoundEntity

mkDealerRoundRelations :: EntityRef ShoeState -> DealerRoundRelations
mkDealerRoundRelations = DealerRoundRelations

data DealerRoundEntity = DealerRoundEntity
  { _tick :: Tick,
    _state :: DealerRoundState,
    _rels :: DealerRoundRelations
  }
  deriving (Eq, Show, Generic)

data DealerRoundState = DealerRoundState
  { _roundNumber :: Int,
    _isActive :: Bool
  }
  deriving (Eq, Show, Generic)

data DealerRoundRelations = DealerRoundRelations
  { _shoeUsed :: EntityRef ShoeState
  }
  deriving (Eq, Show, Generic)

data DealerRoundStateDelta
  = SetDealerRoundNumber Int
  | SetActive Bool
  deriving (Eq, Show, Generic)

data DealerRoundRelationsDelta
  = SetShoeUsed (EntityRef ShoeState)
  deriving (Eq, Show, Generic)

data DealerRoundEntityDelta
  = DealerRoundStateDelta DealerRoundStateDelta
  | DealerRoundRelationsDelta DealerRoundRelationsDelta
  deriving (Eq, Show, Generic)

instance Clocked DealerRoundEntity where
  tick = _tick
  setTick t e = e {_tick = t}

instance Reversible DealerRoundStateDelta where
  invert = \case
    SetDealerRoundNumber _ -> Left NotInvertible
    SetActive b -> Right (SetActive (not b))

instance Reversible DealerRoundRelationsDelta where
  invert = \case
    SetShoeUsed _ -> Left NotInvertible

instance Reversible DealerRoundEntityDelta where
  invert = \case
    DealerRoundStateDelta d -> DealerRoundStateDelta <$> invert d
    DealerRoundRelationsDelta d -> DealerRoundRelationsDelta <$> invert d

instance ToJSON DealerRoundEntity

instance FromJSON DealerRoundEntity

instance ToJSON DealerRoundState

instance FromJSON DealerRoundState

instance ToJSON DealerRoundRelations

instance FromJSON DealerRoundRelations

instance ToJSON DealerRoundStateDelta

instance FromJSON DealerRoundStateDelta

instance ToJSON DealerRoundRelationsDelta

instance FromJSON DealerRoundRelationsDelta

instance ToJSON DealerRoundEntityDelta

instance FromJSON DealerRoundEntityDelta

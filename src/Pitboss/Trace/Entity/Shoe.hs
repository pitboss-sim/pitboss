{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Pitboss.Trace.Entity.Shoe where

import Data.Aeson
import GHC.Generics (Generic)
import Pitboss.Blackjack.Card (Card)
import Pitboss.Trace.Entity.Capabilities.Clocked
import Pitboss.Trace.Entity.Capabilities.Reversible

data ShoeState = ShoeState
  { _shoeCards :: [Card]
  }
  deriving (Eq, Show, Generic)

data ShoeEntity = ShoeEntity
  { _tick :: Tick,
    _state :: ShoeState
  }
  deriving (Eq, Show, Generic)

mkShoeEntity :: Tick -> [Card] -> ShoeEntity
mkShoeEntity t cards = ShoeEntity t (ShoeState cards)

instance Clocked ShoeEntity where
  tick = _tick
  setTick t s = s {_tick = t}

data ShoeEntityDelta = NoShoeDelta
  deriving (Eq, Show, Generic)

instance ToJSON ShoeEntity

instance FromJSON ShoeEntity

instance ToJSON ShoeState

instance FromJSON ShoeState

instance ToJSON ShoeEntityDelta

instance FromJSON ShoeEntityDelta

instance Reversible ShoeEntityDelta where
  invert = \case
    NoShoeDelta -> Right NoShoeDelta

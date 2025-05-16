{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Delta.DealerRound.State where

import Data.Aeson
import GHC.Generics
import Pitboss.Trace.Delta.Capabilities.Incremental
import Pitboss.Trace.Delta.Capabilities.Reversible
import Pitboss.Trace.Entity.DealerRound

data DealerRoundStateDelta
  = SetDealerRoundNumber Int
  | SetActive Bool
  deriving (Eq, Show, Generic)

instance ToJSON DealerRoundStateDelta

instance FromJSON DealerRoundStateDelta

instance Incremental DealerRoundStateDelta where
  type Entity DealerRoundStateDelta = DealerRoundState

  applyDelta delta state = case delta of
    SetDealerRoundNumber n -> state {_roundNumber = n}
    SetActive b -> state {_isActive = b}

  previewDelta delta entity = Just $ applyDelta delta entity

  describeDelta delta _ = case delta of
    SetActive b -> "Set round active: " ++ show b
    SetDealerRoundNumber n -> "Set round number to " ++ show n

instance Reversible DealerRoundStateDelta where
  invert = \case
    SetDealerRoundNumber _ -> Left NotInvertible
    SetActive b -> Right (SetActive (not b))

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.DealerRound.Delta.Attrs where

import Data.Aeson
import GHC.Generics
import Pitboss.Trace.Entity.Capabilities
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
    SetDealerRoundNumber n -> state {_dealerRoundStateNumber = n}
    SetActive b -> state {_dealerRoundStateIsActive = b}

  previewDelta delta entity = Just $ applyDelta delta entity

  describeDelta delta _ = case delta of
    SetActive b -> "Set round active: " ++ show b
    SetDealerRoundNumber n -> "Set round number to " ++ show n

instance Reversible DealerRoundStateDelta where
  invert = \case
    SetDealerRoundNumber _ -> Left NotInvertible
    SetActive b -> Right (SetActive (not b))

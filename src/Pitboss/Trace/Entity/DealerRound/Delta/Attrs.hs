{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.DealerRound.Delta.Attrs where

import Data.Aeson
import GHC.Generics
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.DealerRound

data DealerRoundEntityAttrsDelta
  = SetDealerRoundEntityNumber Int
  | SetActive Bool
  deriving (Eq, Show, Generic)

instance ToJSON DealerRoundEntityAttrsDelta

instance FromJSON DealerRoundEntityAttrsDelta

instance Incremental DealerRoundEntityAttrsDelta where
  type Target DealerRoundEntityAttrsDelta = DealerRoundEntityAttrs

  applyDelta delta state = case delta of
    SetDealerRoundEntityNumber n -> state {_dealerRoundEntityAttrsNumber = n}
    SetActive b -> state {_dealerRoundEntityAttrsIsActive = b}

  previewDelta delta entity = Just $ applyDelta delta entity

  describeDelta delta _ = case delta of
    SetActive b -> "Set round active: " ++ show b
    SetDealerRoundEntityNumber n -> "Set round number to " ++ show n

instance Reversible DealerRoundEntityAttrsDelta where
  invert = \case
    SetDealerRoundEntityNumber _ -> Left NotInvertible
    SetActive b -> Right (SetActive (not b))

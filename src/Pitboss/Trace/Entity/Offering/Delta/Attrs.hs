{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.Offering.Delta.Attrs where

import Data.Aeson
import GHC.Generics
import Pitboss.Blackjack.Offering as O
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.Offering

data OfferingStateDelta
  = ReplaceOffering O.Offering O.Offering
  deriving (Eq, Show, Generic)

instance ToJSON OfferingStateDelta

instance FromJSON OfferingStateDelta

instance Reversible OfferingStateDelta where
  invert = \case
    ReplaceOffering old new -> Right (ReplaceOffering new old)

instance Incremental OfferingStateDelta where
  type Entity OfferingStateDelta = OfferingState

  applyDelta delta state = case delta of
    ReplaceOffering _ new -> state {_offeringStateOffering = new}

  previewDelta delta state = case delta of
    ReplaceOffering old _ ->
      if old == _offeringStateOffering state
        then Just $ applyDelta delta state
        else Nothing

  describeDelta _ _ = "Replaced offering (details omitted)"

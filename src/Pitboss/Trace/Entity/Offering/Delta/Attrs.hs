{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.Offering.Delta.Attrs where

import Data.Aeson
import GHC.Generics
import Pitboss.Blackjack.Offering as O
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.Offering

data OfferingEntityAttrsDelta
  = ReplaceOffering O.Offering O.Offering
  deriving (Eq, Show, Generic)

instance ToJSON OfferingEntityAttrsDelta

instance FromJSON OfferingEntityAttrsDelta

instance Reversible OfferingEntityAttrsDelta where
  invert = \case
    ReplaceOffering old new -> Right (ReplaceOffering new old)

instance Incremental OfferingEntityAttrsDelta where
  type Target OfferingEntityAttrsDelta = OfferingEntityAttrs

  applyDelta delta state = case delta of
    ReplaceOffering _ new -> state {_offeringEntityAttrsOffering = new}

  previewDelta delta state = case delta of
    ReplaceOffering old _ ->
      if old == _offeringEntityAttrsOffering state
        then Just $ applyDelta delta state
        else Nothing

  describeDelta _ _ = "Replaced offering (details omitted)"

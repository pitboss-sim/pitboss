{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.Offering.Delta.Attrs where

import Data.Aeson
import GHC.Generics
import Pitboss.Blackjack.Offering as O
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.Offering.Types

data OfferingEntityAttrsDelta
    = ReplaceOffering O.Offering O.Offering
    deriving (Eq, Show, Generic)

instance ToJSON OfferingEntityAttrsDelta
instance FromJSON OfferingEntityAttrsDelta

instance Incremental OfferingEntityAttrsDelta where
    type Target OfferingEntityAttrsDelta = OfferingEntityAttrs

    applyDelta :: OfferingEntityAttrsDelta -> OfferingEntityAttrs -> OfferingEntityAttrs
    applyDelta (ReplaceOffering _ new) state =
        state{_offeringEntityAttrsOffering = new}

    previewDelta :: OfferingEntityAttrsDelta -> OfferingEntityAttrs -> Maybe OfferingEntityAttrs
    previewDelta (ReplaceOffering old _) state =
        if _offeringEntityAttrsOffering state == old
            then Just (applyDelta (ReplaceOffering old old) state)
            else Nothing

    describeDelta :: OfferingEntityAttrsDelta -> OfferingEntityAttrs -> String
    describeDelta _ _ = "Replaced offering (details omitted)"

instance Reversible OfferingEntityAttrsDelta where
    invert = \case
        ReplaceOffering old new -> Right (ReplaceOffering new old)

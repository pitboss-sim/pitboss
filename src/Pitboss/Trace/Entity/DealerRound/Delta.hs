{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Pitboss.Trace.Entity.DealerRound.Delta (
    module Pitboss.Trace.Entity.DealerRound.Delta.Attrs,
    module Pitboss.Trace.Entity.DealerRound.Delta.Modes,
    module Pitboss.Trace.Entity.DealerRound.Delta.Rels,
    DealerRoundEntityDelta (..),
)
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.DealerRound
import Pitboss.Trace.Entity.DealerRound.Delta.Attrs
import Pitboss.Trace.Entity.DealerRound.Delta.Modes
import Pitboss.Trace.Entity.DealerRound.Delta.Rels

data DealerRoundEntityDelta
    = DealerRoundEntityAttrsDelta DealerRoundEntityAttrsDelta
    | DealerRoundEntityRelsDelta DealerRoundEntityRelsDelta
    deriving (Eq, Show, Generic)

instance ToJSON DealerRoundEntityDelta

instance FromJSON DealerRoundEntityDelta

instance Incremental DealerRoundEntityDelta where
    type Target DealerRoundEntityDelta = DealerRoundEntity

    applyDelta delta entity = case delta of
        DealerRoundEntityAttrsDelta rd -> entity{_dealerRoundEntityAttrs = applyDelta rd (_dealerRoundEntityAttrs entity)}
        DealerRoundEntityRelsDelta sd -> entity{_dealerRoundEntityRels = applyDelta sd (_dealerRoundEntityRels entity)}

    previewDelta delta e = Just $ applyDelta delta e

    describeDelta delta _ = case delta of
        DealerRoundEntityAttrsDelta (SetDealerRoundEntityNumber n) -> "Set round number to " ++ show n
        DealerRoundEntityAttrsDelta (SetActive b) -> "Set round active: " ++ show b
        DealerRoundEntityRelsDelta _ -> "Updated shoe used"

instance Reversible DealerRoundEntityDelta where
    invert = \case
        DealerRoundEntityAttrsDelta delta -> DealerRoundEntityAttrsDelta <$> invert delta
        DealerRoundEntityRelsDelta delta -> DealerRoundEntityRelsDelta <$> invert delta

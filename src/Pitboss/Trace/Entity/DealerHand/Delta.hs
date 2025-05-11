{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.DealerHand.Delta
  ( module Pitboss.Trace.Entity.DealerHand.Delta.Attrs,
    module Pitboss.Trace.Entity.DealerHand.Delta.Modes,
    module Pitboss.Trace.Entity.DealerHand.Delta.Rels,
    DealerHandEntityDelta (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.DealerHand
import Pitboss.Trace.Entity.DealerHand.Delta.Attrs
import Pitboss.Trace.Entity.DealerHand.Delta.Modes
import Pitboss.Trace.Entity.DealerHand.Delta.Rels

data DealerHandEntityDelta
  = DealerHandEntityAttrsDelta DealerHandEntityAttrsDelta
  | DealerHandEntityModesDelta DealerHandEntityModesDelta
  | DealerHandEntityRelsDelta DealerHandEntityRelsDelta
  deriving (Eq, Show, Generic)

instance ToJSON DealerHandEntityDelta

instance FromJSON DealerHandEntityDelta

instance Incremental DealerHandEntityDelta where
  type Entity DealerHandEntityDelta = DealerHandEntity

  applyDelta delta entity = case delta of
    DealerHandEntityAttrsDelta d -> entity {_dealerHandEntityAttrs = applyDelta d (_dealerHandEntityAttrs entity)}
    DealerHandEntityModesDelta d -> entity {_dealerHandEntityModes = applyDelta d (_dealerHandEntityModes entity)}
    DealerHandEntityRelsDelta d -> entity {_dealerHandEntityRels = applyDelta d (_dealerHandEntityRels entity)}

  previewDelta delta entity = Just $ applyDelta delta entity

  describeDelta delta entity = case delta of
    DealerHandEntityAttrsDelta sd -> describeDelta sd (_dealerHandEntityAttrs entity)
    DealerHandEntityModesDelta _ -> "Modes replaced"
    DealerHandEntityRelsDelta rd -> describeDelta rd (_dealerHandEntityRels entity)

instance Reversible DealerHandEntityDelta where
  invert = \case
    DealerHandEntityAttrsDelta d -> DealerHandEntityAttrsDelta <$> invert d
    DealerHandEntityModesDelta d -> DealerHandEntityModesDelta <$> invert d
    DealerHandEntityRelsDelta d -> DealerHandEntityRelsDelta <$> invert d

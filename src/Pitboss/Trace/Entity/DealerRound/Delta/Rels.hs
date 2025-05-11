{-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Pitboss.Trace.Entity.DealerRound.Delta.Rels where

import Data.Aeson
import GHC.Generics
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.DealerRound
import Pitboss.Trace.Types.EntityRef
import Pitboss.Trace.Types.Identifier

data DealerRoundEntityRelsDelta
  = SetTableShoeUsed (EntityRef TableShoeEntityId)
  deriving (Eq, Show, Generic)

instance ToJSON DealerRoundEntityRelsDelta

instance FromJSON DealerRoundEntityRelsDelta

instance Incremental DealerRoundEntityRelsDelta where
  type Entity DealerRoundEntityRelsDelta = DealerRoundEntityRels

  applyDelta delta rels = case delta of
    SetTableShoeUsed shoe -> rels {_dealerRoundEntityRelsTableShoeUsed = shoe}

  previewDelta delta rels = Just $ applyDelta delta rels

  describeDelta _ _ = "Updated shoe used"

instance Reversible DealerRoundEntityRelsDelta where
  invert = \case
    SetTableShoeUsed _ -> Left NotInvertible

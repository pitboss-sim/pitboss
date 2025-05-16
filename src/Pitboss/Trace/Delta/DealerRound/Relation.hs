{-# LANGUAGE LambdaCase #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Pitboss.Trace.Delta.DealerRound.Relation where

import Data.Aeson
import GHC.Generics
import Pitboss.Trace.Delta.Capabilities.Incremental
import Pitboss.Trace.Delta.Capabilities.Reversible
import Pitboss.Trace.Entity.DealerRound
import Pitboss.Trace.Types.EntityRef
import Pitboss.Trace.Types.Identifier

data DealerRoundRelationsDelta
  = SetShoeUsed (EntityRef ShoeId)
  deriving (Eq, Show, Generic)

instance ToJSON DealerRoundRelationsDelta

instance FromJSON DealerRoundRelationsDelta

instance Incremental DealerRoundRelationsDelta where
  type Entity DealerRoundRelationsDelta = DealerRoundRelations

  applyDelta delta rels = case delta of
    SetShoeUsed shoe -> rels {_shoeUsed = shoe}

  previewDelta delta rels = Just $ applyDelta delta rels

  describeDelta _ _ = "Updated shoe used"

instance Reversible DealerRoundRelationsDelta where
  invert = \case
    SetShoeUsed _ -> Left NotInvertible

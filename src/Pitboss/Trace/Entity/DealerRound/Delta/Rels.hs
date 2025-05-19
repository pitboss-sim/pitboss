{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Pitboss.Trace.Entity.DealerRound.Delta.Rels where

import Data.Aeson
import GHC.Generics
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.DealerRound.Types
import Pitboss.Trace.Entity.Types.EntityId

data DealerRoundEntityRelsDelta
    = SetTableShoeUsed (ClockedRef TableShoeEntityId)
    deriving (Eq, Show, Generic)

instance ToJSON DealerRoundEntityRelsDelta
instance FromJSON DealerRoundEntityRelsDelta

instance Incremental DealerRoundEntityRelsDelta where
    type Target DealerRoundEntityRelsDelta = DealerRoundEntityRels

    applyDelta :: DealerRoundEntityRelsDelta -> DealerRoundEntityRels -> DealerRoundEntityRels
    applyDelta delta rels = case delta of
        SetTableShoeUsed shoe -> rels{_dealerRoundEntityRelsTableShoeUsed = shoe}

    previewDelta :: DealerRoundEntityRelsDelta -> DealerRoundEntityRels -> Maybe DealerRoundEntityRels
    previewDelta delta rels = Just $ applyDelta delta rels

    describeDelta :: DealerRoundEntityRelsDelta -> DealerRoundEntityRels -> String
    describeDelta (SetTableShoeUsed new) _ = "Updated shoe used to " ++ show new

instance Reversible DealerRoundEntityRelsDelta where
    invert = \case
        SetTableShoeUsed _ -> Left NotInvertible

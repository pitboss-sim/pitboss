{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.DealerHand.Delta.Modes where

import Data.Aeson
import GHC.Generics
import Pitboss.FSM.DealerHand
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.DealerHand

data DealerHandEntityModesDelta
    = ReplaceFSM SomeDealerHandFSM SomeDealerHandFSM
    deriving (Eq, Show, Generic)

instance Incremental DealerHandEntityModesDelta where
    type Target DealerHandEntityModesDelta = DealerHandEntityModes

    applyDelta (ReplaceFSM _ new) entity = entity{_dealerHandEntityModesDealerHand = new}

    previewDelta delta entity = Just $ applyDelta delta entity

    describeDelta _ new = "FSM replaced" ++ show new

instance ToJSON DealerHandEntityModesDelta

instance FromJSON DealerHandEntityModesDelta

instance Reversible DealerHandEntityModesDelta where
    invert (ReplaceFSM old new) = Right (ReplaceFSM new old)

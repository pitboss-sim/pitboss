{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.DealerRound.Delta.Modes where

import Data.Aeson
import GHC.Generics
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.DealerRound

data DealerRoundEntityModesDelta
  = Nothing
  deriving (Eq, Show, Generic)

instance Incremental DealerRoundEntityModesDelta where
  type Entity DealerRoundEntityModesDelta = DealerRoundEntityModes

  applyDelta _ _ = undefined
  previewDelta delta entity = Just $ applyDelta delta entity
  describeDelta _ _ = undefined

instance ToJSON DealerRoundEntityModesDelta where
  toJSON = undefined

instance FromJSON DealerRoundEntityModesDelta where
  parseJSON = undefined

instance Reversible DealerRoundEntityModesDelta where
  invert = undefined

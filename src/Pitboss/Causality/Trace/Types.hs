{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Pitboss.Causality.Trace.Types where

import Data.Aeson.Types
import GHC.Generics (Generic)
import Pitboss.Blackjack hiding (Hand)
import Pitboss.Causality.Types.Core

data family DeathReason (k :: EntityKind)

data instance DeathReason 'Bout = BoutComplete DetailedOutcome
    deriving (Eq, Show, Generic)

instance ToJSON (DeathReason 'Bout)
instance FromJSON (DeathReason 'Bout)

data instance DeathReason 'Hand = HandResolved HandResolution
    deriving (Eq, Show, Generic)

instance ToJSON (DeathReason 'Hand)
instance FromJSON (DeathReason 'Hand)

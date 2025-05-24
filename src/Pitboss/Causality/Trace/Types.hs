{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Pitboss.Causality.Trace.Types where

import Data.Aeson.Types
import GHC.Generics (Generic)
import Pitboss.Blackjack hiding (Hand)
import Pitboss.Causality.Types.Core

data family DeathReason (k :: EntityKind)

data instance DeathReason 'Bout = BoutComplete DetailedOutcome
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

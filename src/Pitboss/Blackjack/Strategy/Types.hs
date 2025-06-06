{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Pitboss.Blackjack.Strategy.Types where

import Data.Aeson.Types
import GHC.Generics (Generic)
import Pitboss.Blackjack.Types.Core

newtype Fallback
    = Else Move
    deriving stock (Eq, Show, Generic)
    deriving newtype (ToJSON, FromJSON)

data Decision
    = Always Move
    | Prefer Move Fallback
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

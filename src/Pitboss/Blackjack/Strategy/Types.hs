module Pitboss.Blackjack.Strategy.Types where

import Pitboss.Blackjack.Types.Core

newtype Fallback
    = Else Move
    deriving (Eq, Show)

data Decision
    = Always Move
    | Prefer Move Fallback
    deriving (Eq, Show)

module Pitboss.Blackjack.BasicStrategy.Types where

import Pitboss.Blackjack.Actions

newtype Fallback
    = Else Move
    deriving (Eq, Show)

data Decision
    = Always Move
    | Prefer Move Fallback
    deriving (Eq, Show)

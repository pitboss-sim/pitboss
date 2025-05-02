module Pitboss.Strategy.Types where

newtype Fallback
  = Else Move
  deriving (Eq, Show)

data Decision
  = Always Move
  | Prefer Move Fallback
  deriving (Eq, Show)

data Move
  = Hit
  | Stand
  | Double
  | Split
  | Surrender
  deriving (Eq, Show)

module Pitboss.Blackjack.Action
  ( PlayerAction (..),
    DealerAction (..),
  )
where

data PlayerAction
  = Hit
  | Stand
  | Double
  | Split
  | Surrender
  | TakeInsurance
  | DeclineInsurance
  deriving (Eq, Show, Ord, Enum, Bounded)

data DealerAction
  = DealerHit
  | DealerStand
  deriving (Eq, Show, Ord, Enum, Bounded)

module Pitboss.Blackjack.Action where

data PlayerAction
    = Hit
    | Stand
    | Double
    | Split
    | Surrender
    | TakeInsurance
    | DeclineInsurance
    deriving (Eq, Show, Ord, Enum)

data DealerAction
    = DealerHit
    | DealerStand
    deriving (Eq, Show, Ord, Enum)

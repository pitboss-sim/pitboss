module Pitboss.FSM.Bout.Phase where

data BoutPhase
    = AwaitingFirstCard
    | AwaitingSecondCard
    | PlayerTurn
    | DealerTurn
    | Settlement
    | Done
    deriving (Eq, Show)

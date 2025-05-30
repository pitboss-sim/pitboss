module Pitboss.FSM.Bout.Phase where

data BoutPhase
    = BAwaitingFirstCard
    | BAwaitingSecondCard
    | BPlayerTurn
    | BDealerTurn
    | BSettlement
    | BDone
    deriving (Eq, Show)

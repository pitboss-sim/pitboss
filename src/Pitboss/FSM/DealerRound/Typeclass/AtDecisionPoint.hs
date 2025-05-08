module Pitboss.FSM.DealerRound.Typeclass.AtDecisionPoint where

class AtDecisionPoint fsm where
    toPlayersPhase :: fsm -> Maybe fsm

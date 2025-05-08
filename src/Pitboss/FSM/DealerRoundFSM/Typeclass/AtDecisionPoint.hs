module Pitboss.FSM.DealerRoundFSM.Typeclass.AtDecisionPoint where

class AtDecisionPoint fsm where
  toPlayersPhase :: fsm -> Maybe fsm

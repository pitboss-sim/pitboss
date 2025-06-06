{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Pitboss.FSM.Round.AtDecisionPoint where

import Pitboss.FSM.Types

class AtDecisionPoint fsm where
    toPlayersPhase :: fsm -> Maybe fsm

instance AtDecisionPoint (ENHCFSM p) where
    toPlayersPhase = \case
        ENHCPlayersFSM -> Just ENHCPlayersFSM
        _ -> Nothing

instance AtDecisionPoint (PeekFSM p) where
    toPlayersPhase = \case
        PeekBoutPlayersFSM -> Just PeekBoutPlayersFSM
        _ -> Nothing

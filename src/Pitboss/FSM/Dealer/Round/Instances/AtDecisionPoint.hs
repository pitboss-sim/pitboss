{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Pitboss.FSM.Dealer.Round.Instances.AtDecisionPoint where

import Pitboss.FSM

class AtDecisionPoint fsm where
    toPlayersPhase :: fsm -> Maybe fsm

instance AtDecisionPoint (ENHCFSM p) where
    toPlayersPhase = \case
        ENHCPlayersFSM -> Just ENHCPlayersFSM
        _ -> Nothing

instance AtDecisionPoint (PeekFSM p) where
    toPlayersPhase = \case
        PeekPlayersFSM -> Just PeekPlayersFSM
        _ -> Nothing

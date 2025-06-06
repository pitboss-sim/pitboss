{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Pitboss.FSM.Dealer.Round.AtDecisionPoint where

import Pitboss.FSM.Dealer.Round.ENHC
import Pitboss.FSM.Dealer.Round.Peek

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

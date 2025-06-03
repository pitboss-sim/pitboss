module Pitboss.Sim.Engine.DeltaGen where

import Control.Monad.Reader
import Pitboss.Blackjack hiding (Stand)
import Pitboss.FSM
import Pitboss.Sim.Event
import Pitboss.State

generateDeltas :: BlackjackEvent -> CausalHistory -> Reader TickCacheContext [TraceOp]
generateDeltas event history = case event of
    PlayerStood _playerId handId -> do
        maybeHand <- deref handId
        case maybeHand of
            Just hand -> do
                let oldFSM = _phFsm (_phModes hand)
                    newFSM = SomePlayerHandFSM (PHResolvedFSM PHStand)
                    delta = ModesDelta history (DPlayerHandSetPlayerHandFSM newFSM oldFSM)
                    mutation = createMutation handId hand delta
                pure [mutation]
            Nothing -> pure []
    PlayerHit _playerId _handId -> do
        pure []
    CardDealt _card (ToPlayerHand _handId) -> do
        pure []
    CardDealt _card (ToDealerHand _handId) -> do
        pure []
    BoutSettled _boutId detailedOutcome -> do
        case outcome detailedOutcome of
            PlayerWins -> pure []
            DealerWins -> pure []
            Push -> pure []
    DealerRevealed _dealerId _handId -> do
        pure []
    _ -> pure []

module Pitboss.Sim.Engine.DeltaGen where

import Control.Monad.Reader
import Pitboss.Blackjack.Events
import Pitboss.FSM.PlayerHand hiding (Push)
import Pitboss.State.Delta.Types
import Pitboss.State.Entity.Types
import Pitboss.State.TickCache
import Pitboss.State.Trace.Ops

generateDeltas :: BlackjackEvent -> CausalHistory -> Reader TickCacheContext [TraceOp]
generateDeltas event history = case event of
    PlayerStood _playerId handId -> do
        maybeHand <- deref handId
        case maybeHand of
            Just hand -> do
                let oldFSM = _phFsm (_phModes hand)
                    newFSM = SomePlayerHandFSM (ResolvedFSM Stand)
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
    BoutSettled _boutId outcome -> do
        case outcome of
            PlayerWins -> pure []
            DealerWins -> pure []
            Push -> pure []
    DealerRevealed _dealerId _handId -> do
        pure []
    _ -> pure []

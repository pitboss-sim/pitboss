{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Pitboss.Simulation.Engine.DeltaGen where

import Control.Monad.Reader
import Pitboss.Blackjack hiding (Stand)
import Pitboss.Causality
import Pitboss.FSM
import Pitboss.Simulation.Event

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
    PlayerHit _playerId handId -> do
        maybeHand <- deref handId
        case maybeHand of
            Just hand -> do
                let oldFSM = _phFsm (_phModes hand)
                    newFSM = case oldFSM of
                        SomePlayerHandFSM PHDecisionFSM -> SomePlayerHandFSM PHHittingFSM
                        other -> other
                    delta = ModesDelta history (DPlayerHandSetPlayerHandFSM newFSM oldFSM)
                    mutation = createMutation handId hand delta
                pure [mutation]
            Nothing -> pure []
    CardDealt card (ToPlayerHand handId) -> do
        maybeHand <- deref handId
        case maybeHand of
            Just hand -> do
                let oldHand = _phAttrsHand (_phAttrs hand)
                    oldCards = case oldHand of
                        SomeHand h -> handCards h
                    newCards = oldCards ++ [card]
                    newHand = characterize newCards

                    attrDelta = AttrsDelta history (DPlayerHandSetHand newHand oldHand)
                    attrMutation = createMutation handId hand attrDelta

                    oldFSM = _phFsm (_phModes hand)
                    fsmOps = case (oldFSM, newHand) of
                        (SomePlayerHandFSM PHHittingFSM, SomeHand h) ->
                            case witness h of
                                BustWitness ->
                                    let newFSM = SomePlayerHandFSM (PHResolvedFSM PHBust)
                                        modeDelta = ModesDelta history (DPlayerHandSetPlayerHandFSM newFSM oldFSM)
                                     in [createMutation handId hand modeDelta]
                                _ -> []
                        (SomePlayerHandFSM PHDecisionFSM, SomeHand h) ->
                            case (witness h, length newCards) of
                                (BlackjackWitness, 2) ->
                                    let newFSM = SomePlayerHandFSM PHBlackjackFSM
                                        modeDelta = ModesDelta history (DPlayerHandSetPlayerHandFSM newFSM oldFSM)
                                     in [createMutation handId hand modeDelta]
                                _ -> []
                        _ -> []

                pure $ attrMutation : fsmOps
            Nothing -> pure []
    CardDealt card (ToDealerHand handId) -> do
        maybeHand <- deref handId
        case maybeHand of
            Just hand -> do
                let oldHand = _dhAttrsHand (_dhAttrs hand)
                    oldCards = case oldHand of
                        SomeHand h -> handCards h
                    newCards = oldCards ++ [card]
                    newHand = characterize newCards

                    attrDelta = AttrsDelta history (DDealerHandSetHand newHand oldHand)
                    mutation = createMutation handId hand attrDelta

                pure [mutation]
            Nothing -> pure []
    BoutSettled boutId detailedOutcome -> do
        maybeBout <- deref boutId
        case maybeBout of
            Just bout -> do
                let oldOutcome = _boutAttrsOutcome (_boutAttrs bout)
                    newOutcome = Just detailedOutcome
                    delta = AttrsDelta history (DBoutSetOutcome newOutcome oldOutcome)
                    mutation = createMutation boutId bout delta
                pure [mutation]
            Nothing -> pure []
    DealerRevealed _dealerId handId -> do
        maybeHand <- deref handId
        case maybeHand of
            Just hand -> do
                let oldFSM = _dhModesDealerHand (_dhModes hand)
                    newFSM = SomeDealerHandFSM DHEvaluatingFSM
                    delta = ModesDelta history (DDealerHandSetFSM newFSM oldFSM)
                    mutation = createMutation handId hand delta
                pure [mutation]
            Nothing -> pure []
    PlayerDoubledDown _playerId handId -> do
        maybeHand <- deref handId
        case maybeHand of
            Just hand -> do
                let oldFSM = _phFsm (_phModes hand)
                    newFSM = SomePlayerHandFSM (PHOneCardDrawFSM PHDouble)
                    delta = ModesDelta history (DPlayerHandSetPlayerHandFSM newFSM oldFSM)
                    mutation = createMutation handId hand delta
                pure [mutation]
            Nothing -> pure []
    PlayerSplit _playerId _handId -> do
        -- TODO: Implement split logic
        pure []
    PlayerSurrender _playerId handId -> do
        maybeHand <- deref handId
        case maybeHand of
            Just hand -> do
                let oldFSM = _phFsm (_phModes hand)
                    newFSM = SomePlayerHandFSM (PHResolvedFSM PHSurrendered)
                    delta = ModesDelta history (DPlayerHandSetPlayerHandFSM newFSM oldFSM)
                    mutation = createMutation handId hand delta
                pure [mutation]
            Nothing -> pure []

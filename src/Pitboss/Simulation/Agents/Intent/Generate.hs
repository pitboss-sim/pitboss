{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Pitboss.Simulation.Agents.Intent.Generate where

import Control.Monad.Reader
import Control.Monad.State

import Pitboss.Blackjack
import Pitboss.Causality
import Pitboss.FSM
import Pitboss.Simulation.Agents.Archetype.Player.Advantage
import Pitboss.Simulation.Agents.Archetype.Player.BasicStrategy
import Pitboss.Simulation.Agents.Archetype.Player.Perfect
import Pitboss.Simulation.Agents.Archetype.Player.Superstitious
import Pitboss.Simulation.Agents.Types
import Pitboss.Simulation.Event
import System.Random (StdGen)

generatePlayerHandIntent ::
    SomePlayerArchetype ->
    EntityId 'Player ->
    EntityId 'PlayerHand ->
    StdGen ->
    Reader TickCacheContext (Maybe BlackjackEvent)
generatePlayerHandIntent archetype playerId handId gen = do
    maybeHand <- deref handId
    case maybeHand of
        Nothing -> pure Nothing
        Just hand -> case _phFsm (_phModes hand) of
            SomePlayerHandFSM PHDecisionFSM -> do
                contextM <- buildContext hand
                case contextM of
                    Nothing -> pure Nothing
                    Just ctx -> do
                        let move = evalState (decideByArchetype archetype ctx) gen
                        pure $ Just $ moveToEvent playerId handId move
            _ -> pure Nothing

buildContext :: EntityState 'PlayerHand -> Reader TickCacheContext (Maybe GameContext)
buildContext hand = do
    let boutId = _phRelsBelongsToBout (_phRels hand)
    maybeBout <- deref boutId

    case maybeBout of
        Nothing -> pure Nothing
        Just bout -> do
            let dealerHandId = _boutRelsDealerHand (_boutRels bout)
            maybeDealerHand <- deref dealerHandId

            let tableId = _boutRelsTable (_boutRels bout)
            maybeTable <- deref tableId

            case (maybeDealerHand, maybeTable) of
                (Just dealerHand, Just table) -> do
                    let dealerCards = case _dhAttrsHand (_dhAttrs dealerHand) of
                            SomeHand hand' -> handCards hand'
                        dealerUpcard = case dealerCards of
                            (c : _) -> c
                            [] -> error "Dealer hand has no cards - invalid game state"

                        offering = _tAttrsOffering (_tAttrs table)

                    pure $
                        Just
                            GameContext
                                { _contextPlayerHand = _phAttrsHand (_phAttrs hand)
                                , _contextDealerUpcard = dealerUpcard
                                , _contextOffering = offering
                                , _contextCanDouble = _phAttrsHandIx (_phAttrs hand) == 0
                                , _contextCanSplit = case extractPairRank (_phAttrsHand (_phAttrs hand)) of
                                    Just _ -> _phAttrsSplitDepth (_phAttrs hand) < 3
                                    Nothing -> False
                                , _contextCanSurrender = _phAttrsSplitDepth (_phAttrs hand) == 0
                                , _contextHandNumber = _phAttrsHandIx (_phAttrs hand)
                                , _contextSplitCount = _phAttrsSplitDepth (_phAttrs hand)
                                }
                _ -> pure Nothing

moveToEvent :: EntityId 'Player -> EntityId 'PlayerHand -> Move -> BlackjackEvent
moveToEvent playerId handId = \case
    Stand -> PlayerStood playerId handId
    Hit -> PlayerHit playerId handId
    Double -> PlayerDoubledDown playerId handId
    Split -> PlayerSplit playerId handId
    Surrender -> PlayerSurrender playerId handId

decideByArchetype :: SomePlayerArchetype -> GameContext -> State StdGen Move
decideByArchetype archetype ctx = case archetype of
    SomePlayerBasicStrategy arch ->
        getBasicStrategyMove (bsConfig arch) ctx
    SomePlayerPerfect arch ->
        getPerfectMove (pfConfig arch) ctx
    SomePlayerAdvantage arch ->
        getAdvantageMove (advConfig arch) (advState arch) ctx
    SomePlayerSuperstitious arch ->
        getSuperstitiousMove (ssConfig arch) ctx

generateDealerIntent ::
    EntityId 'Dealer ->
    EntityId 'DealerHand ->
    Reader TickCacheContext (Maybe BlackjackEvent)
generateDealerIntent _dealerId handId = do
    maybeDealerHand <- deref handId
    case maybeDealerHand of
        Nothing -> pure Nothing
        Just dealerHand -> case _dhModesDealerHand (_dhModes dealerHand) of
            SomeDealerHandFSM DHDealingFSM -> pure Nothing -- TODO: implement when dealer hand gets bout reference
            _ -> pure Nothing

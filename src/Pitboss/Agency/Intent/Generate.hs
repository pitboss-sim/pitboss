{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Pitboss.Agency.Intent.Generate where

import Control.Monad.Reader
import Control.Monad.State

import Pitboss.Agency.Archetype.Player.BasicStrategy (getBasicStrategyMove)
import Pitboss.Agency.Archetype.Types
import Pitboss.Agency.Types
import Pitboss.Blackjack.Action as Action
import Pitboss.Blackjack.Events as Events
import Pitboss.Blackjack.Materia.Card (Card (..), Rank (..), Suit (..))
import Pitboss.Blackjack.Materia.Hand (extractPairRank)
import Pitboss.Blackjack.Offering.WellKnown (vegas6)
import Pitboss.FSM.PlayerHand
import Pitboss.State.Entity.Types
import Pitboss.State.TickCache
import Pitboss.State.Types.Core
import System.Random (StdGen, mkStdGen)

generatePlayerHandIntent ::
    EntityId 'Player ->
    EntityId 'PlayerHand ->
    Reader TickCacheContext (Maybe BlackjackEvent)
generatePlayerHandIntent playerId handId = do
    maybeHand <- deref handId
    case maybeHand of
        Nothing -> pure Nothing
        Just hand -> case _phFsm (_phModes hand) of
            SomePlayerHandFSM DecisionFSM -> do
                -- Hand needs a decision
                maybePlayer <- deref playerId
                case maybePlayer of
                    Nothing -> pure Nothing
                    Just player -> do
                        -- Get the decision
                        move <- decideHandAction player hand
                        pure $ Just $ moveToEvent playerId handId move
            _ ->
                -- Hand doesn't need a decision right now
                pure Nothing

moveToEvent :: EntityId 'Player -> EntityId 'PlayerHand -> Move -> BlackjackEvent
moveToEvent playerId handId = \case
    Action.Stand -> Events.PlayerStood playerId handId
    Action.Hit -> Events.PlayerHit playerId handId
    Action.Double -> Events.PlayerDoubledDown playerId handId
    Action.Split -> Events.PlayerSplit playerId handId
    Action.Surrender -> Events.PlayerSurrender playerId handId

decideHandAction :: EntityState 'Player -> EntityState 'PlayerHand -> Reader TickCacheContext Move
decideHandAction player hand = do
    ctx <- buildMinimalContext hand

    let archetype = _pAttrsArchetype (_pAttrs player)

    let gen = mkStdGen 42 -- TODO: Get from simulation state
    pure $ evalState (decideByArchetype archetype ctx) gen

buildMinimalContext :: EntityState 'PlayerHand -> Reader TickCacheContext GameContext
buildMinimalContext hand = do
    let dealerUpcard = Card Ace Hearts -- TODO: Look up from bout
        offering = vegas6 -- TODO: Look up from table
    pure
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

decideByArchetype :: SomePlayerArchetype -> GameContext -> State StdGen Move
decideByArchetype archetype ctx = case archetype of
    SomePlayerBasicStrategy arch ->
        getBasicStrategyMove (bsConfig arch) ctx
    SomePlayerPerfect _ ->
        pure Action.Stand -- TODO: Implement
    SomePlayerAdvantage _ ->
        pure Action.Stand -- TODO: Implement
    SomePlayerSuperstitious _ ->
        pure Action.Stand -- TODO: Implement

generateDealerIntent :: EntityId 'DealerHand -> Reader TickCacheContext (Maybe BlackjackEvent)
generateDealerIntent _handId = do
    pure Nothing

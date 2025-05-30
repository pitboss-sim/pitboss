{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Pitboss.Agency.Intent.Generate where

import Control.Monad.Reader
import Pitboss.Agency.Archetype.Types
import Pitboss.Blackjack.Action as Action
import Pitboss.Blackjack.Events as Events
import Pitboss.Blackjack.Materia.Hand
import Pitboss.FSM.PlayerHand hiding (Double, Stand, Surrender)
import Pitboss.State.Entity.Types
import Pitboss.State.TickCache
import Pitboss.State.Types.Core

-- Generate intent for a player's hand
generatePlayerHandIntent :: EntityId 'Player -> EntityId 'PlayerHand -> Reader TickCacheContext (Maybe BlackjackEvent)
generatePlayerHandIntent playerId handId = do
    maybeHand <- deref handId
    maybePlayer <- deref playerId

    case (maybeHand, maybePlayer) of
        (Just hand, Just player) ->
            case _phFsm (_phModes hand) of
                SomePlayerHandFSM DecisionFSM -> do
                    -- Use player's archetype to decide
                    let archetype = _pAttrsArchetype (_pAttrs player)
                        handCards' = _phAttrsHand (_phAttrs hand)
                    decision <- decidePlayerAction archetype handCards'
                    pure $ Just $ case decision of
                        Action.Stand -> Events.PlayerStood playerId handId
                        Action.Hit -> Events.PlayerHit playerId handId
                        Action.Double -> Events.PlayerDoubledDown playerId handId
                        Action.Split -> Events.PlayerSplit playerId handId
                        Action.Surrender -> Events.PlayerSurrender playerId handId
                _ -> pure Nothing
        _ -> pure Nothing

-- Generate intent for dealer
generateDealerIntent :: EntityId 'Dealer -> Reader TickCacheContext (Maybe BlackjackEvent)
generateDealerIntent _dealerId = do
    -- Check if dealer needs to reveal, hit, etc.
    -- This would look at dealer hand FSM state
    pure Nothing

-- Generate intent for table (dealing cards)
generateTableIntent :: EntityId 'Table -> Reader TickCacheContext (Maybe BlackjackEvent)
generateTableIntent _tableId = do
    -- Check bout states to see what needs dealing
    -- Would generate CardDealt events
    pure Nothing

-- Helper to decide player action based on archetype
decidePlayerAction :: SomePlayerArchetype -> SomeHand -> Reader TickCacheContext Move
decidePlayerAction _archetype _hand = do
    -- Dispatch to archetype-specific strategy
    pure Action.Stand -- TODO: Implement actual strategy

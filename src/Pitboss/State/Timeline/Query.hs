{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.State.Timeline.Query where

import Data.HashMap.Strict.InsOrd qualified as IHM
import Data.Word (Word64)
import Pitboss.State.Delta.Types
import Pitboss.State.Entity.Types
import Pitboss.State.Registry
import Pitboss.State.Timeline
import Pitboss.State.Timeline.Reconstruction
import Pitboss.State.Types.Core

uidToRegistryKey :: Uid k -> Word64
uidToRegistryKey (Uid (_, EntityId entropy)) = entropy

entityIdToWord64 :: EntityId k -> Word64
entityIdToWord64 (EntityId entropy) = entropy

lookupInRegistry ::
    forall k.
    Registry k (SomeDelta k) ->
    Uid k ->
    Maybe (Timeline k (SomeDelta k))
lookupInRegistry (Registry registry) uid =
    IHM.lookup (entityIdToWord64 (uidEntityId uid)) registry

lookupDealerAtTick ::
    Registry 'Dealer (SomeDelta 'Dealer) ->
    Uid 'Dealer ->
    Tick ->
    Maybe (EntityState 'Dealer 'TransactionBoundary)
lookupDealerAtTick registry uid tick = do
    timeline <- lookupInRegistry registry uid
    reconstructDealerAt timeline tick

lookupPlayerAtTick ::
    Registry 'Player (SomeDelta 'Player) ->
    Uid 'Player ->
    Tick ->
    Maybe (EntityState 'Player 'TransactionBoundary)
lookupPlayerAtTick registry uid tick = do
    timeline <- lookupInRegistry registry uid
    reconstructPlayerAt timeline tick
lookupDealerHandAtTick ::
    Registry 'DealerHand (SomeDelta 'DealerHand) ->
    Uid 'DealerHand ->
    Tick ->
    Maybe (EntityState 'DealerHand 'TransactionBoundary)
lookupDealerHandAtTick registry uid tick = do
    timeline <- lookupInRegistry registry uid
    reconstructDealerHandAt timeline tick

lookupDealerRoundAtTick ::
    Registry 'DealerRound (SomeDelta 'DealerRound) ->
    Uid 'DealerRound ->
    Tick ->
    Maybe (EntityState 'DealerRound 'TransactionBoundary)
lookupDealerRoundAtTick registry uid tick = do
    timeline <- lookupInRegistry registry uid
    reconstructDealerRoundAt timeline tick

lookupOfferingAtTick ::
    Registry 'Offering (SomeDelta 'Offering) ->
    Uid 'Offering ->
    Tick ->
    Maybe (EntityState 'Offering 'TransactionBoundary)
lookupOfferingAtTick registry uid tick = do
    timeline <- lookupInRegistry registry uid
    reconstructOfferingAt timeline tick

lookupPlayerHandAtTick ::
    Registry 'PlayerHand (SomeDelta 'PlayerHand) ->
    Uid 'PlayerHand ->
    Tick ->
    Maybe (EntityState 'PlayerHand 'TransactionBoundary)
lookupPlayerHandAtTick registry uid tick = do
    timeline <- lookupInRegistry registry uid
    reconstructPlayerHandAt timeline tick

lookupPlayerSpotAtTick ::
    Registry 'PlayerSpot (SomeDelta 'PlayerSpot) ->
    Uid 'PlayerSpot ->
    Tick ->
    Maybe (EntityState 'PlayerSpot 'TransactionBoundary)
lookupPlayerSpotAtTick registry uid tick = do
    timeline <- lookupInRegistry registry uid
    reconstructPlayerSpotAt timeline tick

lookupTableAtTick ::
    Registry 'Table (SomeDelta 'Table) ->
    Uid 'Table ->
    Tick ->
    Maybe (EntityState 'Table 'TransactionBoundary)
lookupTableAtTick registry uid tick = do
    timeline <- lookupInRegistry registry uid
    reconstructTableAt timeline tick

lookupTableShoeAtTick ::
    Registry 'TableShoe (SomeDelta 'TableShoe) ->
    Uid 'TableShoe ->
    Tick ->
    Maybe (EntityState 'TableShoe 'TransactionBoundary)
lookupTableShoeAtTick registry uid tick = do
    timeline <- lookupInRegistry registry uid
    reconstructTableShoeAt timeline tick

findLastCompleteTransaction :: [SomeDelta k] -> ([SomeDelta k], [SomeDelta k])
findLastCompleteTransaction deltas =
    case break isBoundary (reverse deltas) of
        (_, []) -> ([], deltas)
        (incomplete, boundary : complete) ->
            (reverse (boundary : complete), reverse incomplete)
  where
    isBoundary (Boundary _) = True
    isBoundary _ = False

lookupDealerAtTick :: Registry 'Dealer (SomeDelta 'Dealer) -> Uid 'Dealer -> Tick -> Maybe (EntityState 'Dealer 'TransactionBoundary)
lookupDealerAtTick = lookupEntityAtTick

lookupPlayerAtTick :: Registry 'Player (SomeDelta 'Player) -> Uid 'Player -> Tick -> Maybe (EntityState 'Player 'TransactionBoundary)
lookupPlayerAtTick = lookupEntityAtTick

lookupDealerHandAtTick :: Registry 'DealerHand (SomeDelta 'DealerHand) -> Uid 'DealerHand -> Tick -> Maybe (EntityState 'DealerHand 'TransactionBoundary)
lookupDealerHandAtTick = lookupEntityAtTick

lookupTableAtTick :: Registry 'Table (SomeDelta 'Table) -> Uid 'Table -> Tick -> Maybe (EntityState 'Table 'TransactionBoundary)
lookupTableAtTick = lookupEntityAtTick

lookupEntityAtTick :: (ComposeEntity k) => Registry k (SomeDelta k) -> Uid k -> Tick -> Maybe (EntityState k 'TransactionBoundary)
lookupEntityAtTick registry uid tick = do
    timeline <- lookupInRegistry registry uid
    reconstructAt timeline tick

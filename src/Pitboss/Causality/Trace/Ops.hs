{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Pitboss.Causality.Trace.Ops where

import Control.Lens
import Data.HashMap.Strict.InsOrd qualified as IHM
import Pitboss.Causality.Delta.Types
import Pitboss.Causality.Entity.Types
import Pitboss.Causality.Registry
import Pitboss.Causality.Timeline
import Pitboss.Causality.Trace
import Pitboss.Causality.Trace.Types
import Pitboss.Causality.Types.Core

data TraceOp where
    BirthOp :: EntityKindWitness k -> EntityId k -> EntityState k -> TraceOp
    MutationOp :: EntityKindWitness k -> EntityId k -> SomeDelta k -> TraceOp
    DeathOp :: EntityKindWitness k -> EntityId k -> DeathReason k -> TraceOp

class HasRegistry (k :: EntityKind) where
    registryLens :: Lens' Trace (Registry k (SomeDelta k))

instance HasRegistry 'Bout where
    registryLens = bouts

instance HasRegistry 'Table where
    registryLens = tables

instance HasRegistry 'TableShoe where
    registryLens = tableShoes

instance HasRegistry 'Dealer where
    registryLens = dealers

instance HasRegistry 'DealerHand where
    registryLens = dealerHands

instance HasRegistry 'DealerRound where
    registryLens = dealerRounds

instance HasRegistry 'Player where
    registryLens = players

instance HasRegistry 'PlayerSpot where
    registryLens = playerSpots

instance HasRegistry 'PlayerHand where
    registryLens = playerHands

applyBirthTyped :: forall k. (HasRegistry k) => EntityId k -> EntityState k -> Tick -> Trace -> Trace
applyBirthTyped eid state tick trace =
    let timeline = mkTimeline eid tick state
        Registry reg = trace ^. registryLens @k
        updatedReg = Registry $ IHM.insert eid timeline reg
     in trace & registryLens @k .~ updatedReg

applyMutationTyped :: forall k. (HasRegistry k) => EntityId k -> SomeDelta k -> Tick -> Trace -> Trace
applyMutationTyped eid someDelta tick trace =
    let Registry reg = trace ^. registryLens @k
     in case IHM.lookup eid reg of
            Just timeline ->
                let updatedTimeline =
                        timeline
                            { timelineDeltas = IHM.insertWith (++) tick [someDelta] (timelineDeltas timeline)
                            }
                    updatedReg = Registry $ IHM.insert eid updatedTimeline reg
                 in trace & registryLens @k .~ updatedReg
            Nothing -> trace

applyTraceOp :: TraceOp -> Tick -> Trace -> Trace
applyTraceOp (BirthOp witness' eid state) tick trace =
    case witness' of
        BoutWitness -> applyBirthTyped @'Bout eid state tick trace
        PlayerWitness -> applyBirthTyped @'Player eid state tick trace
        DealerWitness -> applyBirthTyped @'Dealer eid state tick trace
        PlayerHandWitness -> applyBirthTyped @'PlayerHand eid state tick trace
        DealerHandWitness -> applyBirthTyped @'DealerHand eid state tick trace
        PlayerSpotWitness -> applyBirthTyped @'PlayerSpot eid state tick trace
        DealerRoundWitness -> applyBirthTyped @'DealerRound eid state tick trace
        TableWitness -> applyBirthTyped @'Table eid state tick trace
        TableShoeWitness -> applyBirthTyped @'TableShoe eid state tick trace
applyTraceOp (MutationOp witness' eid someDelta) tick trace =
    case witness' of
        BoutWitness -> applyMutationTyped @'Bout eid someDelta tick trace
        PlayerWitness -> applyMutationTyped @'Player eid someDelta tick trace
        DealerWitness -> applyMutationTyped @'Dealer eid someDelta tick trace
        PlayerHandWitness -> applyMutationTyped @'PlayerHand eid someDelta tick trace
        DealerHandWitness -> applyMutationTyped @'DealerHand eid someDelta tick trace
        PlayerSpotWitness -> applyMutationTyped @'PlayerSpot eid someDelta tick trace
        DealerRoundWitness -> applyMutationTyped @'DealerRound eid someDelta tick trace
        TableWitness -> applyMutationTyped @'Table eid someDelta tick trace
        TableShoeWitness -> applyMutationTyped @'TableShoe eid someDelta tick trace
applyTraceOp (DeathOp _witness _eid _reason) _tick trace = trace -- TODO: implement death handling

instance Show TraceOp where
    show (BirthOp witness' eid _state) =
        "BirthOp " ++ show witness' ++ " " ++ show eid ++ " <state>"
    show (MutationOp witness' eid _delta) =
        "MutationOp " ++ show witness' ++ " " ++ show eid ++ " <delta>"
    show (DeathOp witness' eid _reason) =
        "DeathOp " ++ show witness' ++ " " ++ show eid ++ " <reason>"

createBirth :: (HasWitness k) => EntityId k -> EntityState k -> TraceOp
createBirth eid state = BirthOp (witness state) eid state

createMutation :: (HasWitness k) => EntityId k -> EntityState k -> SomeDelta k -> TraceOp
createMutation eid state = MutationOp (witness state) eid

createDeath :: (HasWitness k) => EntityId k -> EntityState k -> DeathReason k -> TraceOp
createDeath eid state = DeathOp (witness state) eid

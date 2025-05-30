{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.State.Timeline.Reconstruction where

import Control.Monad (foldM)
import Data.HashMap.Strict.InsOrd qualified as IHM
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Pitboss.State.Delta.Instances.Incremental
import Pitboss.State.Delta.Types
import Pitboss.State.Entity.Types
import Pitboss.State.Timeline
import Pitboss.State.Types.Core

collectDeltasUpTo :: Timeline k (SomeDelta k) -> Tick -> [SomeDelta k]
collectDeltasUpTo timeline targetTick =
    let relevantTicks = filter (<= targetTick) (sort (IHM.keys (timelineDeltas timeline)))
        allDeltas = concatMap (\t -> fromMaybe [] (IHM.lookup t (timelineDeltas timeline))) relevantTicks
     in allDeltas

-- Simple approach: find the last complete entity state from transaction boundaries
-- and apply subsequent deltas from there
reconstructAt ::
    (IncrementalWithWitness k) =>
    Timeline k (SomeDelta k) ->
    Tick ->
    Maybe (EntityState k)
reconstructAt timeline tick =
    let allDeltas = collectDeltasUpTo timeline tick
     in case timelineInitialState timeline of
            Nothing -> Nothing -- No initial state, can't reconstruct
            Just initialState ->
                -- Start with initial state and apply all deltas up to tick
                foldM applyDeltaToEntity initialState allDeltas
  where
    applyDeltaToEntity :: (IncrementalWithWitness k) => EntityState k -> SomeDelta k -> Maybe (EntityState k)
    applyDeltaToEntity entity delta = case delta of
        AttrsDelta _ d -> Just $ applyWithWitness AttrsWitness d entity
        ModesDelta _ d -> Just $ applyWithWitness ModesWitness d entity
        RelsDelta _ d -> Just $ applyWithWitness RelsWitness d entity
        BoundaryDelta _ _ -> Just entity -- Boundaries don't mutate state

findLastCompleteTransaction :: [SomeDelta k] -> ([SomeDelta k], [SomeDelta k])
findLastCompleteTransaction deltas =
    case break isBoundary (reverse deltas) of
        (_, []) -> ([], deltas) -- No boundary found
        (incomplete, boundary : complete) ->
            (reverse (boundary : complete), reverse incomplete)
  where
    isBoundary (BoundaryDelta _ _) = True
    isBoundary _ = False

-- This needs to be implemented based on how we want to handle
-- initial entity creation from transaction boundaries
reconstructFromBoundaries :: [SomeDelta k] -> Maybe (EntityState k)
reconstructFromBoundaries _ = Nothing -- Placeholder - needs implementation

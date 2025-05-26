{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.State.Timeline.Reconstruction where

import Data.HashMap.Strict.InsOrd qualified as IHM
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Pitboss.State.Delta.Instances.Incremental
import Pitboss.State.Delta.Types
import Pitboss.State.Entity.Types
import Pitboss.State.Timeline
import Pitboss.State.Types.Core
import Control.Monad (foldM)

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
        (completeDeltas, incompleteDeltas) = findLastCompleteTransaction allDeltas
    in case completeDeltas of
        [] -> Nothing -- No complete entity state available
        _ -> do
            baseEntity <- reconstructFromBoundaries completeDeltas
            foldM applyDeltaToEntity baseEntity incompleteDeltas
  where
    applyDeltaToEntity :: (IncrementalWithWitness k) => EntityState k -> SomeDelta k -> Maybe (EntityState k)
    applyDeltaToEntity entity delta = case delta of
        AttrsUpdate d -> Just $ applyWithWitness AttrsWitness d entity
        ModesUpdate d -> Just $ applyWithWitness ModesWitness d entity
        RelsUpdate d -> Just $ applyWithWitness RelsWitness d entity
        Boundary _ -> Just entity

findLastCompleteTransaction :: [SomeDelta k] -> ([SomeDelta k], [SomeDelta k])
findLastCompleteTransaction deltas =
    case break isBoundary (reverse deltas) of
        (_, []) -> ([], deltas) -- No boundary found
        (incomplete, boundary : complete) ->
            (reverse (boundary : complete), reverse incomplete)
  where
    isBoundary (Boundary _) = True
    isBoundary _ = False

-- This needs to be implemented based on how we want to handle
-- initial entity creation from transaction boundaries
reconstructFromBoundaries :: [SomeDelta k] -> Maybe (EntityState k)
reconstructFromBoundaries _ = Nothing -- Placeholder - needs implementation


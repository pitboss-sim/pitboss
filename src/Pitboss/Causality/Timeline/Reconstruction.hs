{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Causality.Timeline.Reconstruction where

import Control.Monad (foldM)
import Data.HashMap.Strict.InsOrd qualified as IHM
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Pitboss.Causality.Delta.Incremental
import Pitboss.Causality.Delta.Types
import Pitboss.Causality.Entity.Types
import Pitboss.Causality.Timeline
import Pitboss.Causality.Types.Core

collectDeltasUpTo :: Timeline k (SomeDelta k) -> Tick -> [SomeDelta k]
collectDeltasUpTo timeline targetTick =
    let relevantTicks = filter (<= targetTick) (sort (IHM.keys (timelineDeltas timeline)))
        allDeltas = concatMap (\t -> fromMaybe [] (IHM.lookup t (timelineDeltas timeline))) relevantTicks
     in allDeltas

reconstructAt ::
    (IncrementalWithWitness k) =>
    Timeline k (SomeDelta k) ->
    Tick ->
    Maybe (EntityState k)
reconstructAt timeline tick =
    let allDeltas = collectDeltasUpTo timeline tick
     in case timelineInitialState timeline of
            Nothing -> Nothing
            Just initialState ->
                foldM applyDeltaToEntity initialState allDeltas
  where
    applyDeltaToEntity :: (IncrementalWithWitness k) => EntityState k -> SomeDelta k -> Maybe (EntityState k)
    applyDeltaToEntity entity delta = case delta of
        AttrsDelta _ d -> Just $ applyWithWitness AttrsWitness d entity
        ModesDelta _ d -> Just $ applyWithWitness ModesWitness d entity
        RelsDelta _ d -> Just $ applyWithWitness RelsWitness d entity
        BoundaryDelta _ _ -> Just entity

reconstructFromBoundaries :: [SomeDelta k] -> Maybe (EntityState k)
reconstructFromBoundaries _ = Nothing -- TODO: implement

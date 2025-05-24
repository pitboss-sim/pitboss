{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Causality.Timeline.Reconstruction where

import Control.Monad (foldM)
import Data.HashMap.Strict.InsOrd qualified as IHM
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Pitboss.Causality.Delta.IncrementalPart
import Pitboss.Causality.Delta.Types
import Pitboss.Causality.Entity.Types
import Pitboss.Causality.Timeline
import Pitboss.Causality.Types.Core

collectDeltasUpTo :: Timeline k (SomeDelta k) -> Tick -> [SomeDelta k]
collectDeltasUpTo timeline targetTick =
    let relevantTicks = filter (<= targetTick) (sort (IHM.keys (timelineDeltas timeline)))
        allDeltas = concatMap (\t -> fromMaybe [] (IHM.lookup t (timelineDeltas timeline))) relevantTicks
     in allDeltas

data ReconstructionError
    = NoInitialState
    | DeltaApplicationFailed String
    | EntityNotInRegistry
    deriving (Eq, Show)

reconstructAt ::
    (IncrementalPart k) =>
    Timeline k (SomeDelta k) ->
    Tick ->
    Either ReconstructionError (EntityState k)
reconstructAt timeline tick = do
    let allDeltas = collectDeltasUpTo timeline tick
    initialState <- case timelineInitialState timeline of
        Nothing -> Left NoInitialState
        Just state -> Right state
    foldM applyDeltaToEntity initialState allDeltas
  where
    applyDeltaToEntity entity delta = case delta of
        AttrsDelta _ d -> Right $ applyPart AttrsWitness d entity
        ModesDelta _ d -> Right $ applyPart ModesWitness d entity
        RelsDelta _ d -> Right $ applyPart RelsWitness d entity
        BoundaryDelta _ _ -> Right entity

reconstructFromBoundaries :: [SomeDelta k] -> Maybe (EntityState k)
reconstructFromBoundaries _ = Nothing -- TODO: implement

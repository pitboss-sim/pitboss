{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.State.Timeline.Reconstruction where

import Control.Monad.Except (MonadError (..))
import Control.Monad.State.Strict
import Data.HashMap.Strict.InsOrd qualified as IHM
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Pitboss.State.Delta.Instances.Incremental
import Pitboss.State.Delta.Types
import Pitboss.State.Entity.Types
import Pitboss.State.Timeline
import Pitboss.State.Types.Core

data ReconstructionPhase = Building | Complete | Failed

newtype ReconstructionM (k :: EntityKind) (phase :: ReconstructionPhase) a
    = ReconstructionM (StateT (PartialEntity k) (Either ReconstructionError) a)
    deriving (Functor, Applicative, Monad)

data PartialEntity k = PartialEntity
    { _partialAttrs :: Maybe (EntityState k (PartialUpdate 'Attrs))
    , _partialModes :: Maybe (EntityState k (PartialUpdate 'Modes))
    , _partialRels :: Maybe (EntityState k (PartialUpdate 'Rels))
    , _appliedDeltas :: [SomeDelta k]
    }

data ReconstructionError
    = MissingRequiredPart EntityStatePart
    | DeltaApplicationFailed String
    | InconsistentState String
    deriving (Eq, Show)

emptyPartial :: PartialEntity k
emptyPartial = PartialEntity Nothing Nothing Nothing []

runReconstruction :: ReconstructionM k phase a -> PartialEntity k -> Either ReconstructionError a
runReconstruction (ReconstructionM action) = evalStateT action

collectDeltasUpTo :: Timeline k (SomeDelta k) -> Tick -> [SomeDelta k]
collectDeltasUpTo timeline targetTick =
    let relevantTicks = filter (<= targetTick) (sort (IHM.keys (timelineDeltas timeline)))
        allDeltas = concatMap (\t -> fromMaybe [] (IHM.lookup t (timelineDeltas timeline))) relevantTicks
     in allDeltas

applyDelta :: (IncrementalWithWitness k) => SomeDelta k -> ReconstructionM k 'Building ()
applyDelta delta = ReconstructionM $ do
    partial <- get
    case delta of
        AttrsUpdate d -> case _partialAttrs partial of
            Nothing -> throwError (MissingRequiredPart Attrs)
            Just current -> do
                let newAttrs = applyWithWitness AttrsWitness d current
                put partial{_partialAttrs = Just newAttrs, _appliedDeltas = delta : _appliedDeltas partial}
        ModesUpdate d -> case _partialModes partial of
            Nothing -> throwError (MissingRequiredPart Modes)
            Just current -> do
                let newModes = applyWithWitness ModesWitness d current
                put partial{_partialModes = Just newModes, _appliedDeltas = delta : _appliedDeltas partial}
        RelsUpdate d -> case _partialRels partial of
            Nothing -> throwError (MissingRequiredPart Rels)
            Just current -> do
                let newRels = applyWithWitness RelsWitness d current
                put partial{_partialRels = Just newRels, _appliedDeltas = delta : _appliedDeltas partial}
        Boundary _ -> pure ()

reconstructAt ::
    (ComposeEntity k, IncrementalWithWitness k) =>
    Timeline k (SomeDelta k) ->
    Tick ->
    Maybe (EntityState k 'TransactionBoundary)
reconstructAt timeline tick =
    case runReconstruction reconstructionProcess emptyPartial of
        Left _err -> Nothing
        Right entity -> Just entity
  where
    reconstructionProcess = do
        let relevantDeltas = collectDeltasUpTo timeline tick
        mapM_ applyDelta relevantDeltas
        completeEntity

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Causality.Timeline.Query where

import Data.HashMap.Strict.InsOrd qualified as IHM
import Data.Hashable (Hashable (..))
import Data.Word (Word64)
import Pitboss.Causality.Delta.IncrementalPart
import Pitboss.Causality.Delta.Types
import Pitboss.Causality.Entity.Types
import Pitboss.Causality.Registry
import Pitboss.Causality.Timeline
import Pitboss.Causality.Timeline.Reconstruction
import Pitboss.Causality.Types.Core

uidToRegistryKey :: (EntityIdClass (EntityIdFor k)) => Uid k -> Word64
uidToRegistryKey (Uid (_, entityId)) = toWord64 entityId

lookupInRegistry ::
    forall k.
    (Hashable (EntityIdFor k)) =>
    Registry k (SomeDelta k) ->
    Uid k ->
    Maybe (Timeline k (SomeDelta k))
lookupInRegistry (Registry registry) uid =
    IHM.lookup (uidEntityId uid) registry

lookupEntityAtTick ::
    (Hashable (EntityIdFor k), IncrementalPart k) =>
    Registry k (SomeDelta k) ->
    Uid k ->
    Tick ->
    Either ReconstructionError (EntityState k)
lookupEntityAtTick registry uid tick =
    case lookupInRegistry registry uid of
        Nothing -> Left EntityNotInRegistry
        Just timeline -> reconstructAt timeline tick

findLastCompleteTransaction :: [SomeDelta k] -> ([SomeDelta k], [SomeDelta k])
findLastCompleteTransaction deltas =
    case break isBoundary (reverse deltas) of
        (_, []) -> ([], deltas)
        (incomplete, boundary : complete) ->
            (reverse (boundary : complete), reverse incomplete)
  where
    isBoundary (BoundaryDelta _ _) = True
    isBoundary _ = False

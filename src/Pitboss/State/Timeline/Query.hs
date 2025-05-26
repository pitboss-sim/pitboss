{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.State.Timeline.Query where

import Data.HashMap.Strict.InsOrd qualified as IHM
import Data.Word (Word64)
import Pitboss.State.Delta.Instances.Incremental
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

lookupEntityAtTick ::
    (IncrementalWithWitness k) =>
    Registry k (SomeDelta k) ->
    Uid k ->
    Tick ->
    Maybe (EntityState k)
lookupEntityAtTick registry uid tick = do
    timeline <- lookupInRegistry registry uid
    reconstructAt timeline tick

findLastCompleteTransaction :: [SomeDelta k] -> ([SomeDelta k], [SomeDelta k])
findLastCompleteTransaction deltas =
    case break isBoundary (reverse deltas) of
        (_, []) -> ([], deltas)
        (incomplete, boundary : complete) ->
            (reverse (boundary : complete), reverse incomplete)
  where
    isBoundary (Boundary _) = True
    isBoundary _ = False

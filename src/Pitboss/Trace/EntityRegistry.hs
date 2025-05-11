{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Pitboss.Trace.EntityRegistry where

-- timeline utilities

import Data.Bits (shiftL, (.|.))
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.HashMap.Strict.InsOrd qualified as IHM
import Data.Word (Word64)
import Pitboss.Trace.Entity.Capabilities.Clocked
import Pitboss.Trace.Entity.Capabilities.DeltaDriven
import Pitboss.Trace.EntityRegistry.Identifier
import Pitboss.Trace.Snapshot

-- entity timeline per type

type EntityRegistry id entity = InsOrdHashMap Word64 (StateSnapshot entity)

-- typed accessors

insertEntity ::
  (HasUid id) =>
  id ->
  entity ->
  EntityRegistry id entity ->
  EntityRegistry id entity
insertEntity ident entity =
  let snapshot = StateSnapshot entity emptyTimeline
   in IHM.insert (toWord64 (getUid ident)) snapshot

incrementallyUpdateEntity ::
  (DeltaDriven entity, ToWord64 entity) =>
  Tick ->
  entity ->
  [Delta entity] ->
  EntityRegistry id entity ->
  EntityRegistry id entity
incrementallyUpdateEntity tick' ent deltas registry =
  let key = toWord64 ent
      updated = case IHM.lookup key registry of
        Just snap -> updateSnapshot tick' deltas snap
        Nothing -> mkSnapshot ent tick' deltas
   in IHM.insert key updated registry

lookupEntity :: (HasUid k) => k -> EntityRegistry k v -> Maybe (StateSnapshot v)
lookupEntity k = IHM.lookup (toWord64 (getUid k))

deleteEntity :: (HasUid k) => k -> EntityRegistry k v -> EntityRegistry k v
deleteEntity k = IHM.delete (toWord64 (getUid k))

listEntities :: EntityRegistry k v -> [StateSnapshot v]
listEntities = IHM.elems

emptyEntityRegistry :: EntityRegistry k v
emptyEntityRegistry = IHM.empty

-- uid extraction

class HasUid a where
  getUid :: a -> Uid

instance HasUid OfferingId where getUid (OfferingId u) = u

instance HasUid DealerId where getUid (DealerId u) = u

instance HasUid PlayerId where getUid (PlayerId u) = u

instance HasUid TableId where getUid (TableId u) = u

instance HasUid ShoeId where getUid (ShoeId u) = u

instance HasUid RoundId where getUid (RoundId u) = u

instance HasUid PlayerSpotId where getUid (PlayerSpotId u) = u

instance HasUid HandId where getUid (HandId u) = u

-- uid conversion

class ToWord64 a where
  toWord64 :: a -> Word64

instance ToWord64 Uid where
  toWord64 (Uid s) =
    case decodeBase32Word64 (takeWhile (/= '-') s) of
      Just w -> w
      Nothing -> error $ "Invalid Uid format: " ++ s

decodeBase32Word64 :: String -> Maybe Word64
decodeBase32Word64 = fmap packBits . mapM decodeBase32Char
  where
    packBits = foldl (\acc n -> (acc `shiftL` 5) .|. fromIntegral n) 0

instance {-# OVERLAPPABLE #-} (HasUid a) => ToWord64 a where
  toWord64 = toWord64 . getUid

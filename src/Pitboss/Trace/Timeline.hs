{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Pitboss.Trace.Timeline where

-- timeline utilities

import Data.Bits (shiftL, (.|.))
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.HashMap.Strict.InsOrd qualified as IHM
import Data.Word (Word64)
import Pitboss.Trace.Timeline.Identifier

-- uid extraction

class HasUid a where
  getUid :: a -> Uid

instance HasUid DealerId where getUid (DealerId u) = u

instance HasUid PlayerId where getUid (PlayerId u) = u

instance HasUid TableId where getUid (TableId u) = u

instance HasUid ShoeId where getUid (ShoeId u) = u

instance HasUid RoundId where getUid (RoundId u) = u

instance HasUid SpotId where getUid (SpotId u) = u

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

-- entity timeline per type

type Timeline k v = InsOrdHashMap Word64 v

-- typed accessors

insertEntity :: (ToWord64 k) => k -> v -> Timeline k v -> Timeline k v
insertEntity k = IHM.insert (toWord64 k)

lookupEntity :: (ToWord64 k) => k -> Timeline k v -> Maybe v
lookupEntity k = IHM.lookup (toWord64 k)

deleteEntity :: (ToWord64 k) => k -> Timeline k v -> Timeline k v
deleteEntity k = IHM.delete (toWord64 k)

listEntities :: Timeline k v -> [v]
listEntities = IHM.elems

-- empty timeline

emptyTimeline :: Timeline k v
emptyTimeline = IHM.empty

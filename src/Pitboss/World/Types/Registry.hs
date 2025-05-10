{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Pitboss.World.Types.Registry where

-- registry utilities

import Data.Bits (shiftL, (.|.))
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.HashMap.Strict.InsOrd qualified as IHM
import Data.Word (Word64)
import Pitboss.World.Types.Identifier

-- uid extraction

class HasUid a where
  getUid :: a -> Uid

instance HasUid ActorId where getUid (ActorId u) = u

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

-- entity registry per type

type Registry k v = InsOrdHashMap Word64 v

-- typed accessors

insertEntity :: (ToWord64 k) => k -> v -> Registry k v -> Registry k v
insertEntity k = IHM.insert (toWord64 k)

lookupEntity :: (ToWord64 k) => k -> Registry k v -> Maybe v
lookupEntity k = IHM.lookup (toWord64 k)

deleteEntity :: (ToWord64 k) => k -> Registry k v -> Registry k v
deleteEntity k = IHM.delete (toWord64 k)

listEntities :: Registry k v -> [v]
listEntities = IHM.elems

-- empty registry

emptyRegistry :: Registry k v
emptyRegistry = IHM.empty

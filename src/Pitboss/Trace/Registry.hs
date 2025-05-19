{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Pitboss.Trace.Registry where

-- timeline utilities

import Data.Aeson
import Data.Bits (shiftL, (.|.))
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.Word (Word64)
import GHC.Generics
import Pitboss.Trace.Entity.Entity
import Pitboss.Trace.Timeline

-- delta timeline per type

newtype Registry id delta = Registry
    { unRegistry :: InsOrdHashMap Word64 (Timeline delta)
    }
    deriving stock (Generic)

deriving instance (Eq delta) => Eq (Registry id delta)

deriving instance (Show delta) => Show (Registry id delta)

instance (ToJSON (Timeline delta)) => ToJSON (Registry id delta) where
    toJSON (Registry m) = toJSON m

instance (FromJSON (Timeline delta)) => FromJSON (Registry id delta) where
    parseJSON v = Registry <$> parseJSON v

instance (Monoid (Timeline delta)) => Semigroup (Registry id delta) where
    Registry a <> Registry b = Registry (a <> b)

instance (Monoid (Timeline delta)) => Monoid (Registry id delta) where
    mempty = Registry mempty

-- uid conversion

class ToWord64 a where
    toWord64 :: a -> Word64

instance ToWord64 String where
    toWord64 s =
        case decodeBase32Word64 (takeWhile (/= '-') s) of
            Just w -> w
            Nothing -> error $ "Invalid Uid format: " ++ s

decodeBase32Word64 :: String -> Maybe Word64
decodeBase32Word64 = fmap packBits . mapM decodeBase32Char
  where
    packBits = foldl (\acc n -> (acc `shiftL` 5) .|. fromIntegral n) 0

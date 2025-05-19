{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Pitboss.State.Types.Core (
    EntityKind (..),
    EntityStatePart (..),
    EntityStateSelector (..),
    Uid (..),
    UidPrefix,
    EntityRef (..),
    Tick (..),
    displayUid,
    parseDisplayUid,
    generateUid,
    uidToWord64,
) where

import Data.Hashable (Hashable (..))
import Data.Word (Word64)

import Data.Aeson (FromJSON (..), FromJSONKey, KeyValue ((.=)), ToJSON (..), ToJSONKey, object, withObject, withText, (.:))
import Data.Bits (Bits ((.|.)), shiftL)
import Data.Char (toUpper)
import Data.Data (Proxy (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Numeric (showIntAtBase)
import System.Random (Random (..), StdGen)

-- entity identification

data EntityKind
    = Dealer
    | DealerHand
    | DealerRound
    | Offering
    | Player
    | PlayerHand
    | PlayerSpot
    | Table
    | TableShoe
    deriving (Eq, Show, Generic)

data EntityStatePart = Attrs | Modes | Rels
    deriving (Eq, Show, Generic)

data EntityStateSelector
    = Whole
    | Part EntityStatePart
    deriving (Eq, Show, Generic)

-- uid

newtype Uid (k :: EntityKind) = Uid String
    deriving (Eq, Ord, Show, Generic)

instance forall k. (KnownSymbol (UidPrefix k)) => ToJSON (Uid k) where
    toJSON = toJSON . displayUid

instance forall k. (KnownSymbol (UidPrefix k)) => FromJSON (Uid k) where
    parseJSON = withText "Uid" $ \t ->
        case parseDisplayUid (T.unpack t) of
            Just uid -> pure uid
            Nothing -> fail "Invalid prefixed UID format"

instance Hashable (Uid k) where
    hashWithSalt salt (Uid k) = hashWithSalt salt (uidToWord64 (Uid k))

generateUid :: EntityKind -> Int -> StdGen -> (Uid k, StdGen)
generateUid _ counter gen =
    let (entropy, gen') = randomR (0, 2 ^ (40 :: Int) - 1) gen
        showBase32Digit :: Int -> Char
        showBase32Digit n
            | n >= 0 && n < length base32Chars = base32Chars !! n
            | otherwise = error $ "Invalid base32 digit: " ++ show n
          where
            base32Chars = "0123456789ABCDEFGHJKMNPQRSTVWXYZ"

        padLeft :: Char -> Int -> String -> String
        padLeft c width str' = replicate (width - length str') c ++ str

        showPaddedBase32 :: Int -> Int -> String
        showPaddedBase32 n width = padLeft '0' width (showIntAtBase 32 showBase32Digit n "")

        tsPart = showPaddedBase32 counter 6
        rndPart = showPaddedBase32 entropy 8
        str = tsPart ++ "-" ++ rndPart

        uid' = if isValidUidFormat str then Just (Uid str) else Nothing
     in case uid' of
            Just uid -> (uid, gen')
            Nothing -> error "Internal error: generated invalid Uid"

parseUid :: forall k. (KnownSymbol (UidPrefix k)) => String -> Maybe (Uid k)
parseUid str = do
    let expectedPrefix = symbolVal (Proxy @(UidPrefix k))
    case break (== '-') str of
        (actualPrefix, '-' : rest)
            | actualPrefix == expectedPrefix ->
                if isValidUidFormat rest
                    then Just (Uid str)
                    else Nothing
        _ -> Nothing

isValidUidFormat :: String -> Bool
isValidUidFormat str' =
    case break (== '-') str' of
        (prefix, '-' : suffix) ->
            length prefix == 6
                && length suffix == 8
                && all isBase32Char prefix
                && all isBase32Char suffix
        _ -> False

uidToWord64 :: Uid k -> Word64
uidToWord64 (Uid s) =
    let
     in case Prelude.break (== '-') s of
            (prefix, '-' : suffix) ->
                let digits = mapMaybe decodeBase32Char (prefix ++ suffix)
                 in foldl (\acc d -> acc `shiftL` 5 .|. fromIntegral d) 0 digits
            _ -> error $ "Invalid Uid: " ++ s

decodeBase32Char :: Char -> Maybe Int
decodeBase32Char = (`Map.lookup` base32Map) . toUpper
  where
    base32Map :: Map Char Int
    base32Map = Map.fromList $ zip "0123456789ABCDEFGHJKMNPQRSTVWXYZ" [0 .. 31]

type family UidPrefix (k :: EntityKind) :: Symbol where
    UidPrefix 'Player = "PLR"
    UidPrefix 'Dealer = "DLR"
    UidPrefix 'Table = "TBL"
    UidPrefix 'PlayerHand = "PHD"
    UidPrefix 'DealerHand = "DHD"
    UidPrefix 'PlayerSpot = "PST"
    UidPrefix 'DealerRound = "DRD"
    UidPrefix 'Offering = "OFF"
    UidPrefix 'TableShoe = "SHO"

displayUid :: forall k. (KnownSymbol (UidPrefix k)) => Uid k -> String
displayUid (Uid core) =
    let prefix = symbolVal (Proxy @(UidPrefix k))
     in prefix ++ "-" ++ core

parseDisplayUid :: forall k. (KnownSymbol (UidPrefix k)) => String -> Maybe (Uid k)
parseDisplayUid str = do
    let expectedPrefix = symbolVal (Proxy @(UidPrefix k))
    case break (== '-') str of
        (actualPrefix, '-' : core)
            | actualPrefix == expectedPrefix -> parseUid @k core
        _ -> Nothing

isBase32Char :: Char -> Bool
isBase32Char c = toUpper c `elem` ("0123456789ABCDEFGHJKMNPQRSTVWXYZ" :: String)

-- time

newtype Tick = Tick Word64
    deriving (Eq, Ord, Show, Hashable, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

data Clocked a = Clocked' Tick a
    deriving (Eq, Ord, Show, Generic)

newtype EntityRef (k :: EntityKind) = Clocked (Uid k)
    deriving (Eq, Ord, Show, Generic)

instance (ToJSON (EntityRef k), ToJSON (Uid k)) => ToJSON (EntityRef k) where
    toJSON (Clocked x) = object ["variant" .= ("Clocked" :: String), "value" .= x]

instance (FromJSON (EntityRef k), FromJSON (Uid k)) => FromJSON (EntityRef k) where
    parseJSON = withObject "EntityRef" $ \o -> do
        variant <- o .: "variant"
        case variant :: String of
            "Clocked" -> Clocked <$> o .: "value"
            _ -> fail ("Unknown EntityRef variant: " ++ variant)

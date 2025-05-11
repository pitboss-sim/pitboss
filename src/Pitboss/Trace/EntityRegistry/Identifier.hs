{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- globally unique identifiers

module Pitboss.Trace.EntityRegistry.Identifier where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Bits (shiftL, (.|.))
import Data.Char (toUpper)
import Data.Hashable (Hashable (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Numeric (showIntAtBase)
import System.Random (StdGen, randomR)

-- base UID

newtype Uid = Uid {unUid :: String}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey)

instance Hashable Uid where
  hashWithSalt salt (Uid s) = hashWithSalt salt (uidToWord64 (Uid s))

mkUid :: String -> Maybe Uid
mkUid s = if isValidUidString s then Just (Uid s) else Nothing

generateUid :: Int -> StdGen -> (Uid, StdGen)
generateUid counter gen =
  let (entropy, gen') = randomR (0, 2 ^ (40 :: Int) - 1) gen
      tsPart = showPaddedBase32 counter 6
      rndPart = showPaddedBase32 entropy 8
      str = tsPart ++ "-" ++ rndPart
   in case mkUid str of
        Just uid -> (uid, gen')
        Nothing -> error "Internal error: generated invalid Uid"

-- identifiers

newtype OfferingId = OfferingId Uid
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Hashable, ToJSON, FromJSON)
  deriving (ToJSONKey, FromJSONKey) via Uid

instance Show OfferingId where show (OfferingId (Uid u)) = "F" ++ u

mkOfferingId :: Uid -> OfferingId
mkOfferingId = OfferingId

newtype DealerId = DealerId Uid
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Hashable, ToJSON, FromJSON)
  deriving (ToJSONKey, FromJSONKey) via Uid

instance Show DealerId where show (DealerId (Uid u)) = "A" ++ u

mkDealerId :: Uid -> DealerId
mkDealerId = DealerId

newtype DealerHandId = DealerHandId Uid
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Hashable, ToJSON, FromJSON)
  deriving (ToJSONKey, FromJSONKey) via Uid

instance Show DealerHandId where show (DealerHandId (Uid u)) = "A" ++ u

mkDealerHandId :: Uid -> DealerHandId
mkDealerHandId = DealerHandId

newtype PlayerId = PlayerId Uid
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Hashable, ToJSON, FromJSON)
  deriving (ToJSONKey, FromJSONKey) via Uid

instance Show PlayerId where show (PlayerId (Uid u)) = "A" ++ u

mkPlayerId :: Uid -> PlayerId
mkPlayerId = PlayerId

newtype PlayerHandId = PlayerHandId Uid
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Hashable, ToJSON, FromJSON)
  deriving (ToJSONKey, FromJSONKey) via Uid

instance Show PlayerHandId where show (PlayerHandId (Uid u)) = "A" ++ u

mkPlayerHandId :: Uid -> PlayerHandId
mkPlayerHandId = PlayerHandId

newtype TableId = TableId Uid
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Hashable, ToJSON, FromJSON)
  deriving (ToJSONKey, FromJSONKey) via Uid

instance Show TableId where show (TableId (Uid u)) = "T" ++ u

mkTableId :: Uid -> TableId
mkTableId = TableId

newtype ShoeId = ShoeId Uid
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Hashable, ToJSON, FromJSON)
  deriving (ToJSONKey, FromJSONKey) via Uid

instance Show ShoeId where show (ShoeId (Uid u)) = "S" ++ u

mkShoeId :: Uid -> ShoeId
mkShoeId = ShoeId

newtype RoundId = RoundId Uid
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Hashable, ToJSON, FromJSON)
  deriving (ToJSONKey, FromJSONKey) via Uid

instance Show RoundId where show (RoundId (Uid u)) = "R" ++ u

mkRoundId :: Uid -> RoundId
mkRoundId = RoundId

newtype PlayerSpotId = PlayerSpotId Uid
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Hashable, ToJSON, FromJSON)
  deriving (ToJSONKey, FromJSONKey) via Uid

instance Show PlayerSpotId where show (PlayerSpotId (Uid u)) = "O" ++ u

mkPlayerSpotId :: Uid -> PlayerSpotId
mkPlayerSpotId = PlayerSpotId

newtype HandId = HandId Uid
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Hashable, ToJSON, FromJSON)
  deriving (ToJSONKey, FromJSONKey) via Uid

instance Show HandId where show (HandId (Uid u)) = "H" ++ u

mkHandId :: Uid -> HandId
mkHandId = HandId

-- Crockford base32 helpers

showPaddedBase32 :: Int -> Int -> String
showPaddedBase32 n width = padLeft '0' width (showIntAtBase 32 showBase32Digit n "")

showBase32Digit :: Int -> Char
showBase32Digit n
  | n >= 0 && n < length base32Chars = base32Chars !! n
  | otherwise = error $ "Invalid base32 digit: " ++ show n
  where
    base32Chars = "0123456789ABCDEFGHJKMNPQRSTVWXYZ"

padLeft :: Char -> Int -> String -> String
padLeft c width str = replicate (width - length str) c ++ str

-- validation

isValidUidString :: String -> Bool
isValidUidString str =
  case break (== '-') str of
    (prefix, '-' : suffix) ->
      length prefix == 6
        && length suffix == 8
        && all isBase32Char prefix
        && all isBase32Char suffix
    _ -> False

isBase32Char :: Char -> Bool
isBase32Char c = toUpper c `elem` "0123456789ABCDEFGHJKMNPQRSTVWXYZ"

parseUid :: String -> Maybe Uid
parseUid = mkUid

-- conversion

uidToWord64 :: Uid -> Word64
uidToWord64 (Uid s) =
  case break (== '-') s of
    (prefix, '-' : suffix) ->
      let digits = mapMaybe decodeBase32Char (prefix ++ suffix)
       in foldl (\acc d -> (acc `shiftL` 5) .|. fromIntegral d) 0 digits
    _ -> error $ "Invalid Uid: " ++ s

base32Map :: Map Char Int
base32Map = Map.fromList $ zip "0123456789ABCDEFGHJKMNPQRSTVWXYZ" [0 .. 31]

decodeBase32Char :: Char -> Maybe Int
decodeBase32Char = (`Map.lookup` base32Map) . toUpper

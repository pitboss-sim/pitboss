{-# LANGUAGE DerivingStrategies #-}

-- globally unique identifiers

module Pitboss.World.Types.Identifier where

import Data.Bits (shiftL, (.|.))
import Data.Char (intToDigit, toUpper)
import Data.Hashable (Hashable (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Word (Word64)
import Numeric (showIntAtBase)
import System.Random (StdGen, randomR)

-- base UID

newtype Uid = Uid {unUid :: String}
  deriving stock (Eq, Ord, Show)

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

newtype ActorId = ActorId Uid deriving stock (Eq, Ord)

instance Show ActorId where show (ActorId (Uid u)) = "A" ++ u

mkActorId :: Uid -> ActorId
mkActorId = ActorId

newtype TableId = TableId Uid deriving stock (Eq, Ord)

instance Show TableId where show (TableId (Uid u)) = "T" ++ u

mkTableId :: Uid -> TableId
mkTableId = TableId

newtype ShoeId = ShoeId Uid deriving stock (Eq, Ord)

instance Show ShoeId where show (ShoeId (Uid u)) = "S" ++ u

mkShoeId :: Uid -> ShoeId
mkShoeId = ShoeId

newtype RoundId = RoundId Uid deriving stock (Eq, Ord)

instance Show RoundId where show (RoundId (Uid u)) = "R" ++ u

mkRoundId :: Uid -> RoundId
mkRoundId = RoundId

newtype SpotId = SpotId Uid deriving stock (Eq, Ord)

instance Show SpotId where show (SpotId (Uid u)) = "O" ++ u

mkSpotId :: Uid -> SpotId
mkSpotId = SpotId

newtype HandId = HandId Uid deriving stock (Eq, Ord)

instance Show HandId where show (HandId (Uid u)) = "H" ++ u

mkHandId :: Uid -> HandId
mkHandId = HandId

-- Crockford base32 helpers

showPaddedBase32 :: Int -> Int -> String
showPaddedBase32 n width = padLeft '0' width (showIntAtBase 32 intToDigit n "")

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

-- hashing

instance Hashable Uid where
  hashWithSalt salt (Uid s) = hashWithSalt salt (uidToWord64 (Uid s))

instance Hashable ActorId where
  hashWithSalt salt (ActorId u) = hashWithSalt salt u

instance Hashable ShoeId where
  hashWithSalt salt (ShoeId u) = hashWithSalt salt u

instance Hashable RoundId where
  hashWithSalt salt (RoundId u) = hashWithSalt salt u

instance Hashable SpotId where
  hashWithSalt salt (SpotId u) = hashWithSalt salt u

instance Hashable HandId where
  hashWithSalt salt (HandId u) = hashWithSalt salt u

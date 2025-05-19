{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Pitboss.Causality.Types.Core where

import Data.Aeson.Types
import Data.Bits (Bits ((.|.)), shiftL)
import Data.Char (toUpper)
import Data.Data (Proxy (..))
import Data.Hashable (Hashable (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Word (Word64)
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Numeric (showIntAtBase)
import Pitboss.Causality.Types.FiniteMap
import System.Random (Random (..), RandomGen)

newtype EntityId (k :: EntityKind) = EntityId Word64
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

type BoutId = EntityId 'Bout
type DealerId = EntityId 'Dealer
type PlayerId = EntityId 'Player
type RoundId = EntityId 'Round
type ShoeId = EntityId 'Shoe
type TableId = EntityId 'Table

class (Show a) => EntityIdClass a where
    fromWord64 :: Word64 -> a
    toWord64 :: a -> Word64

instance EntityIdClass (EntityId k) where
    fromWord64 = EntityId
    toWord64 (EntityId w) = w

type family EntityIdFor (k :: EntityKind) where
    EntityIdFor 'Bout = EntityId 'Bout
    EntityIdFor 'Dealer = EntityId 'Dealer
    EntityIdFor 'Player = EntityId 'Player
    EntityIdFor 'Round = EntityId 'Round
    EntityIdFor 'Shoe = EntityId 'Shoe
    EntityIdFor 'Table = EntityId 'Table

newtype IntentId = IntentId Word64
    deriving stock (Eq, Show, Ord, Generic)
    deriving newtype (ToJSON, FromJSON, Hashable)

newtype EventId = EventId Word64
    deriving stock (Eq, Show, Ord, Generic)
    deriving newtype (ToJSON, FromJSON, Hashable)

data CausalHistory = CausalHistory
    { causalIntent :: Maybe IntentId
    , causalEvent :: Maybe EventId
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data EntityKindWitness (k :: EntityKind) where
    BoutWitness :: EntityKindWitness 'Bout
    DealerWitness :: EntityKindWitness 'Dealer
    PlayerWitness :: EntityKindWitness 'Player
    RoundWitness :: EntityKindWitness 'Round
    ShoeWitness :: EntityKindWitness 'Shoe
    TableWitness :: EntityKindWitness 'Table

instance Show (EntityKindWitness k) where
    show BoutWitness = "BoutWitness"
    show DealerWitness = "DealerWitness"
    show PlayerWitness = "PlayerWitness"
    show RoundWitness = "RoundWitness"
    show ShoeWitness = "ShoeWitness"
    show TableWitness = "TableWitness"

data EntityKind
    = Bout
    | Dealer
    | Player
    | Round
    | Shoe
    | Table
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data EntityStatePart = Attrs | Modes | Rels
    deriving (Eq, Show, Generic)

data IntentType
    = PlayerIntent
    | DealerIntent
    | TableIntent
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data OriginatingEntity
    = FromPlayer PlayerId
    | FromDealer DealerId
    | FromTable TableId
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data ActorRole
    = SeatedPlayer
    | BettingPlayer
    | PlayingPlayer
    | ObservingPlayer
    | BetweenRoundsDealer
    | DealingDealer
    | PlayingDealer
    | SettlingDealer
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data SpineLevel
    = SpineBout
    | SpineDealer
    | SpinePlayer
    | SpineRound
    | SpineShoe
    | SpineTable
    deriving stock (Eq, Ord, Show, Enum, Generic)
    deriving anyclass (ToJSON, FromJSON)

type CardIx = Int

data CardState
    = InHand
    | InDiscard
    | Burned
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey)

data TableSpotIx = TableSpot1 | TableSpot2 | TableSpot3 | TableSpot4 | TableSpot5 | TableSpot6
    deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
    deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey, BoundedEnum)

data HandIx = Hand1 | Hand2 | Hand3 | Hand4
    deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
    deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey, BoundedEnum)

newtype Tick = Tick Word64
    deriving stock (Eq, Ord, Show, Bounded, Generic)
    deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

newtype Uid (k :: EntityKind) = Uid (Tick, EntityIdFor k)

newtype EntityRef (k :: EntityKind) = EntityRef (Uid k)

-- Standalone deriving instances with constraints
deriving instance (Eq (EntityIdFor k)) => Eq (Uid k)
deriving instance (Ord (EntityIdFor k)) => Ord (Uid k)
deriving instance (EntityIdClass (EntityIdFor k), Show (EntityIdFor k)) => Show (Uid k)
deriving instance Generic (Uid k)

deriving instance (Eq (EntityIdFor k)) => Eq (EntityRef k)
deriving instance (Ord (EntityIdFor k)) => Ord (EntityRef k)
deriving instance (EntityIdClass (EntityIdFor k), Show (EntityIdFor k)) => Show (EntityRef k)
deriving instance Generic (EntityRef k)

uidTick :: Uid k -> Tick
uidTick (Uid (tick, _)) = tick

uidEntityId :: Uid k -> EntityIdFor k
uidEntityId (Uid (_, entityId)) = entityId

entityRefTick :: EntityRef k -> Tick
entityRefTick (EntityRef uid) = uidTick uid

entityRefId :: EntityRef k -> EntityIdFor k
entityRefId (EntityRef uid) = uidEntityId uid

generateUid :: (EntityIdClass (EntityIdFor k), RandomGen g) => p -> Tick -> g -> (Uid k, g)
generateUid _ tick gen =
    let (entropy, gen') = randomR (0, 2 ^ (40 :: Int) - 1) gen
     in (Uid (tick, fromWord64 entropy), gen')

-- String encoding/decoding
encodeUid :: (EntityIdClass a) => Tick -> a -> String
encodeUid (Tick tickWord) entityId =
    let tickInt = toInteger tickWord `mod` (32 ^ (6 :: Integer))
        entropyInt = toInteger (toWord64 entityId) `mod` (32 ^ (8 :: Integer))
        tsPart = showPaddedBase32 tickInt 6
        rndPart = showPaddedBase32 entropyInt 8
     in tsPart ++ "-" ++ rndPart

decodeUid :: (EntityIdClass a) => String -> Maybe (Tick, a)
decodeUid str =
    case break (== '-') str of
        (prefix, '-' : suffix)
            | length prefix == 6 && length suffix == 8
            , all isBase32Char prefix && all isBase32Char suffix -> do
                tickDigits <- mapM decodeBase32Char prefix
                entropyDigits <- mapM decodeBase32Char suffix
                let tickWord = foldl (\acc d -> acc `shiftL` 5 .|. fromIntegral d) 0 tickDigits
                    entropy = foldl (\acc d -> acc `shiftL` 5 .|. fromIntegral d) 0 entropyDigits
                pure (Tick tickWord, fromWord64 entropy)
        _ -> Nothing

-- Base32 helpers
showPaddedBase32 :: Integer -> Int -> String
showPaddedBase32 n width =
    let base32Chars = "0123456789ABCDEFGHJKMNPQRSTVWXYZ"
        showBase32Digit i
            | i >= 0 && i < 32 = base32Chars !! i
            | otherwise = error $ "Invalid base32 digit: " ++ show i
        result = showIntAtBase 32 showBase32Digit n ""
        padLeft c w s = replicate (w - length s) c ++ s
     in if length result > width
            then drop (length result - width) result
            else padLeft '0' width result

isBase32Char :: Char -> Bool
isBase32Char c = toUpper c `elem` ("0123456789ABCDEFGHJKMNPQRSTVWXYZ" :: String)

decodeBase32Char :: Char -> Maybe Int
decodeBase32Char = (`Map.lookup` base32Map) . toUpper
  where
    base32Map :: Map Char Int
    base32Map = Map.fromList $ zip "0123456789ABCDEFGHJKMNPQRSTVWXYZ" [0 .. 31]

type family UidPrefix (k :: EntityKind) :: Symbol where
    UidPrefix 'Bout = "BOUT"
    UidPrefix 'Dealer = "DELR"
    UidPrefix 'Player = "PLYR"
    UidPrefix 'Round = "ROND"
    UidPrefix 'Shoe = "SHOE"
    UidPrefix 'Table = "TABL"

displayUid :: forall k. (KnownSymbol (UidPrefix k), EntityIdClass (EntityIdFor k)) => Uid k -> String
displayUid (Uid (tick, entityId)) =
    let prefix = symbolVal (Proxy @(UidPrefix k))
        encoded = encodeUid tick entityId
     in prefix ++ "-" ++ encoded

parseDisplayUid :: forall k. (KnownSymbol (UidPrefix k), EntityIdClass (EntityIdFor k)) => String -> Maybe (Uid k)
parseDisplayUid str = do
    let expectedPrefix = symbolVal (Proxy @(UidPrefix k))
    case break (== '-') str of
        (actualPrefix, '-' : rest)
            | actualPrefix == expectedPrefix -> do
                (tick, entityId) <- decodeUid rest
                pure (Uid (tick, entityId))
        _ -> Nothing

instance forall k. (KnownSymbol (UidPrefix k), EntityIdClass (EntityIdFor k)) => ToJSON (Uid k) where
    toJSON = toJSON . displayUid

instance forall k. (KnownSymbol (UidPrefix k), EntityIdClass (EntityIdFor k)) => FromJSON (Uid k) where
    parseJSON = withText "Uid" $ \t ->
        case parseDisplayUid (T.unpack t) of
            Just uid -> pure uid
            Nothing -> fail "Invalid prefixed UID format"

instance forall k. (KnownSymbol (UidPrefix k), EntityIdClass (EntityIdFor k)) => ToJSON (EntityRef k) where
    toJSON (EntityRef uid) = toJSON uid

instance forall k. (KnownSymbol (UidPrefix k), EntityIdClass (EntityIdFor k)) => FromJSON (EntityRef k) where
    parseJSON v = EntityRef <$> parseJSON v

instance (EntityIdClass (EntityIdFor k), Eq (EntityIdFor k)) => Hashable (Uid k) where
    hashWithSalt salt (Uid (Tick tick, entityId)) =
        hashWithSalt (hashWithSalt salt tick) (toWord64 entityId)

instance (EntityIdClass (EntityIdFor k), Eq (EntityIdFor k)) => Hashable (EntityRef k) where
    hashWithSalt salt (EntityRef uid) = hashWithSalt salt uid

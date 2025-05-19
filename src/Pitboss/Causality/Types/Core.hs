{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Pitboss.Causality.Types.Core where

import Data.Aeson (FromJSON (..), FromJSONKey, ToJSON (..), ToJSONKey (..), withText)
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

newtype IntentId = IntentId Word64
    deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON, Hashable)

newtype EventId = EventId Word64
    deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON, Hashable)

data CausalHistory = CausalHistory
    { causalIntent :: Maybe IntentId
    , causalEvent :: Maybe EventId
    }
    deriving (Eq, Show, Generic)

instance ToJSON CausalHistory
instance FromJSON CausalHistory

data EntityKindWitness (k :: EntityKind) where
    BoutWitness :: EntityKindWitness 'Bout
    PlayerWitness :: EntityKindWitness 'Player
    DealerWitness :: EntityKindWitness 'Dealer
    PlayerHandWitness :: EntityKindWitness 'PlayerHand
    DealerHandWitness :: EntityKindWitness 'DealerHand
    PlayerSpotWitness :: EntityKindWitness 'PlayerSpot
    DealerRoundWitness :: EntityKindWitness 'DealerRound
    TableWitness :: EntityKindWitness 'Table
    TableShoeWitness :: EntityKindWitness 'TableShoe

instance Show (EntityKindWitness k) where
    show BoutWitness = "BoutWitness"
    show PlayerWitness = "PlayerWitness"
    show DealerWitness = "DealerWitness"
    show PlayerHandWitness = "PlayerHandWitness"
    show DealerHandWitness = "DealerHandWitness"
    show PlayerSpotWitness = "PlayerSpotWitness"
    show DealerRoundWitness = "DealerRoundWitness"
    show TableWitness = "TableWitness"
    show TableShoeWitness = "TableShoeWitness"

data EntityKind
    = Intent
    | Event
    | Bout
    | Dealer
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

data IntentType
    = PlayerIntent
    | DealerIntent
    | TableIntent
    deriving (Eq, Show, Generic)

data OriginatingEntity
    = FromPlayer (EntityId 'Player)
    | FromDealer (EntityId 'Dealer)
    | FromTable (EntityId 'Table)
    deriving (Eq, Show, Generic)

instance ToJSON IntentType
instance FromJSON IntentType
instance ToJSON OriginatingEntity
instance FromJSON OriginatingEntity

data IntentKind
    = IPlayerHit
    | IPlayerStand
    | IPlayerDouble
    | IPlayerSplit
    | IPlayerSurrender
    | IDealerHit
    | IDealerStand
    | IDealerDeal
    | IDealerSettleBout
    | IDealerSettleInsurance
    deriving (Eq, Show, Generic)

instance ToJSON IntentKind
instance FromJSON IntentKind

data HandTarget
    = ToPlayer (EntityId 'PlayerHand)
    | ToDealer (EntityId 'DealerHand)
    deriving (Show, Eq)

type CardIx = Int

data CardState
    = InHand
    | InDiscard
    | Burned
    deriving (Eq, Show, Generic)

instance ToJSON CardState
instance FromJSON CardState

data PlayerSpotIx
    = EPlayerSpot1
    | EPlayerSpot2
    | EPlayerSpot3
    | EPlayerSpot4
    deriving (Eq, Show, Ord, Enum, Bounded, Generic)

instance ToJSONKey PlayerSpotIx
instance FromJSONKey PlayerSpotIx
instance ToJSON PlayerSpotIx
instance FromJSON PlayerSpotIx
instance BoundedEnum PlayerSpotIx

data PlayerSpotHandIx
    = EPlayerSpotHand1
    | EPlayerSpotHand2
    | EPlayerSpotHand3
    | EPlayerSpotHand4
    deriving (Eq, Show, Ord, Enum, Bounded, Generic)

instance ToJSONKey PlayerSpotHandIx
instance FromJSONKey PlayerSpotHandIx
instance ToJSON PlayerSpotHandIx
instance FromJSON PlayerSpotHandIx
instance BoundedEnum PlayerSpotHandIx

newtype Tick = Tick Word64
    deriving (Eq, Ord, Show, Hashable, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype EntityId (k :: EntityKind) = EntityId Word64
    deriving (Eq, Ord, Show, Generic)

newtype Uid (k :: EntityKind) = Uid (Tick, EntityId k)
    deriving (Eq, Ord, Show, Generic)

newtype EntityRef (k :: EntityKind) = EntityRef (Uid k)
    deriving (Eq, Ord, Show, Generic)

uidTick :: Uid k -> Tick
uidTick (Uid (tick, _)) = tick

uidEntityId :: Uid k -> EntityId k
uidEntityId (Uid (_, entityId)) = entityId

entityRefTick :: EntityRef k -> Tick
entityRefTick (EntityRef uid) = uidTick uid

entityRefId :: EntityRef k -> EntityId k
entityRefId (EntityRef uid) = uidEntityId uid

generateUid :: (RandomGen g) => p -> Tick -> g -> (Uid k, g)
generateUid _ tick gen =
    let (entropy, gen') = randomR (0, 2 ^ (40 :: Int) - 1) gen
     in (Uid (tick, EntityId entropy), gen')

-- String encoding/decoding
encodeUid :: Tick -> EntityId k -> String
encodeUid (Tick tickWord) (EntityId entropy) =
    let tickInt = fromIntegral tickWord `mod` (32 ^ (6 :: Int))
        entropyInt = fromIntegral entropy `mod` (32 ^ (8 :: Int))
        tsPart = showPaddedBase32 tickInt 6
        rndPart = showPaddedBase32 entropyInt 8
     in tsPart ++ "-" ++ rndPart

decodeUid :: String -> Maybe (Tick, EntityId k)
decodeUid str =
    case break (== '-') str of
        (prefix, '-' : suffix)
            | length prefix == 6 && length suffix == 8
            , all isBase32Char prefix && all isBase32Char suffix -> do
                tickDigits <- mapM decodeBase32Char prefix
                entropyDigits <- mapM decodeBase32Char suffix
                let tickWord = foldl (\acc d -> acc `shiftL` 5 .|. fromIntegral d) 0 tickDigits
                    entropy = foldl (\acc d -> acc `shiftL` 5 .|. fromIntegral d) 0 entropyDigits
                pure (Tick tickWord, EntityId entropy)
        _ -> Nothing

-- Base32 helpers
showPaddedBase32 :: Int -> Int -> String
showPaddedBase32 n width =
    let base32Chars = "0123456789ABCDEFGHJKMNPQRSTVWXYZ"
        showBase32Digit i
            | i >= 0 && i < length base32Chars = base32Chars !! i
            | otherwise = error $ "Invalid base32 digit: " ++ show i
        result = showIntAtBase 32 showBase32Digit n ""
        padLeft c w s = replicate (w - length s) c ++ s
     in -- Take only the rightmost 'width' characters if result is too long
        if length result > width
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
    UidPrefix 'Bout = "BOT"
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
displayUid (Uid (tick, entityId)) =
    let prefix = symbolVal (Proxy @(UidPrefix k))
        encoded = encodeUid tick entityId
     in prefix ++ "-" ++ encoded

parseDisplayUid :: forall k. (KnownSymbol (UidPrefix k)) => String -> Maybe (Uid k)
parseDisplayUid str = do
    let expectedPrefix = symbolVal (Proxy @(UidPrefix k))
    case break (== '-') str of
        (actualPrefix, '-' : rest)
            | actualPrefix == expectedPrefix -> do
                (tick, entityId) <- decodeUid rest
                pure (Uid (tick, entityId))
        _ -> Nothing

instance forall k. (KnownSymbol (UidPrefix k)) => ToJSON (Uid k) where
    toJSON = toJSON . displayUid

instance forall k. (KnownSymbol (UidPrefix k)) => FromJSON (Uid k) where
    parseJSON = withText "Uid" $ \t ->
        case parseDisplayUid (T.unpack t) of
            Just uid -> pure uid
            Nothing -> fail "Invalid prefixed UID format"

instance ToJSONKey (EntityId k)
instance FromJSONKey (EntityId k)

instance ToJSON (EntityId k) where
    toJSON (EntityId entropy) = toJSON entropy

instance FromJSON (EntityId k) where
    parseJSON v = EntityId <$> parseJSON v

instance forall k. (KnownSymbol (UidPrefix k)) => ToJSON (EntityRef k) where
    toJSON (EntityRef uid) = toJSON uid

instance forall k. (KnownSymbol (UidPrefix k)) => FromJSON (EntityRef k) where
    parseJSON v = EntityRef <$> parseJSON v

instance Hashable (EntityId k) where
    hashWithSalt salt (EntityId entropy) = hashWithSalt salt entropy

instance Hashable (Uid k) where
    hashWithSalt salt (Uid (Tick tick, entityId)) =
        hashWithSalt (hashWithSalt salt tick) entityId

instance Hashable (EntityRef k) where
    hashWithSalt salt (EntityRef uid) = hashWithSalt salt uid

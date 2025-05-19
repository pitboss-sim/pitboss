{-# LANGUAGE DataKinds #-}
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
import Pitboss.Causality.Types.FiniteMap (BoundedEnum)
import System.Random (Random (..), RandomGen)

-- Entity ID newtypes
newtype BoutId = BoutId Word64
    deriving (Eq, Ord, Show, Generic, Hashable)

newtype ContestantId = ContestantId Word64
    deriving (Eq, Ord, Show, Generic, Hashable)

newtype DealerId = DealerId Word64
    deriving (Eq, Ord, Show, Generic, Hashable)

newtype HandId = HandId Word64
    deriving (Eq, Ord, Show, Generic, Hashable)

newtype PlayerId = PlayerId Word64
    deriving (Eq, Ord, Show, Generic, Hashable)

newtype RoundId = RoundId Word64
    deriving (Eq, Ord, Show, Generic, Hashable)

newtype ShoeId = ShoeId Word64
    deriving (Eq, Ord, Show, Generic, Hashable)

newtype TableId = TableId Word64
    deriving (Eq, Ord, Show, Generic, Hashable)

class (Show a) => EntityIdClass a where
    fromWord64 :: Word64 -> a
    toWord64 :: a -> Word64

instance EntityIdClass BoutId where
    fromWord64 = BoutId
    toWord64 (BoutId w) = w

instance EntityIdClass ContestantId where
    fromWord64 = ContestantId
    toWord64 (ContestantId w) = w

instance EntityIdClass DealerId where
    fromWord64 = DealerId
    toWord64 (DealerId w) = w

instance EntityIdClass HandId where
    fromWord64 = HandId
    toWord64 (HandId w) = w

instance EntityIdClass PlayerId where
    fromWord64 = PlayerId
    toWord64 (PlayerId w) = w

instance EntityIdClass RoundId where
    fromWord64 = RoundId
    toWord64 (RoundId w) = w

instance EntityIdClass ShoeId where
    fromWord64 = ShoeId
    toWord64 (ShoeId w) = w

instance EntityIdClass TableId where
    fromWord64 = TableId
    toWord64 (TableId w) = w

-- Type families for UIDs
type family EntityIdFor (k :: EntityKind) where
    EntityIdFor 'Bout = BoutId
    EntityIdFor 'Contestant = ContestantId
    EntityIdFor 'Dealer = DealerId
    EntityIdFor 'Hand = HandId
    EntityIdFor 'Player = PlayerId
    EntityIdFor 'Round = RoundId
    EntityIdFor 'Shoe = ShoeId
    EntityIdFor 'Table = TableId

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
    ContestantWitness :: EntityKindWitness 'Contestant
    DealerWitness :: EntityKindWitness 'Dealer
    HandWitness :: EntityKindWitness 'Hand
    PlayerWitness :: EntityKindWitness 'Player
    RoundWitness :: EntityKindWitness 'Round
    ShoeWitness :: EntityKindWitness 'Shoe
    TableWitness :: EntityKindWitness 'Table

instance Show (EntityKindWitness k) where
    show BoutWitness = "BoutWitness"
    show ContestantWitness = "ContestantWitness"
    show DealerWitness = "DealerWitness"
    show HandWitness = "HandWitness"
    show PlayerWitness = "PlayerWitness"
    show RoundWitness = "RoundWitness"
    show ShoeWitness = "ShoeWitness"
    show TableWitness = "TableWitness"

data EntityKind
    = Bout
    | Contestant
    | Dealer
    | Hand
    | Player
    | Round
    | Shoe
    | Table
    deriving (Eq, Show, Generic)

data EntityStatePart = Attrs | Modes | Rels
    deriving (Eq, Show, Generic)

data IntentType
    = PlayerIntent
    | DealerIntent
    | TableIntent
    deriving (Eq, Show, Generic)

data OriginatingEntity
    = FromPlayer PlayerId
    | FromDealer DealerId
    | FromTable TableId
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

type CardIx = Int

data CardState
    = InHand
    | InDiscard
    | Burned
    deriving (Eq, Show, Generic)

instance ToJSON CardState
instance FromJSON CardState

data TableSpotIx = TableSpot1 | TableSpot2 | TableSpot3 | TableSpot4 | TableSpot5 | TableSpot6
    deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance ToJSON TableSpotIx
instance FromJSON TableSpotIx
instance ToJSONKey TableSpotIx
instance FromJSONKey TableSpotIx
instance BoundedEnum TableSpotIx

data HandIx = Hand1 | Hand2 | Hand3 | Hand4
    deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance ToJSON HandIx
instance FromJSON HandIx
instance ToJSONKey HandIx
instance FromJSONKey HandIx
instance BoundedEnum HandIx

newtype Tick = Tick Word64
    deriving (Eq, Ord, Show, Hashable, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype Uid (k :: EntityKind) = Uid (Tick, EntityIdFor k)

newtype EntityRef (k :: EntityKind) = EntityRef (Uid k)

-- Standalone deriving instances with constraints
deriving instance (EntityIdClass (EntityIdFor k), Eq (EntityIdFor k)) => Eq (Uid k)
deriving instance (EntityIdClass (EntityIdFor k), Ord (EntityIdFor k)) => Ord (Uid k)
deriving instance (EntityIdClass (EntityIdFor k), Show (EntityIdFor k)) => Show (Uid k)
deriving instance (EntityIdClass (EntityIdFor k)) => Generic (Uid k)

deriving instance (EntityIdClass (EntityIdFor k), Eq (EntityIdFor k)) => Eq (EntityRef k)
deriving instance (EntityIdClass (EntityIdFor k), Ord (EntityIdFor k)) => Ord (EntityRef k)
deriving instance (EntityIdClass (EntityIdFor k), Show (EntityIdFor k)) => Show (EntityRef k)
deriving instance (EntityIdClass (EntityIdFor k)) => Generic (EntityRef k)

uidTick :: Uid k -> Tick
uidTick (Uid (tick, _)) = tick

uidEntityId :: (EntityIdClass (EntityIdFor k)) => Uid k -> EntityIdFor k
uidEntityId (Uid (_, entityId)) = entityId

entityRefTick :: EntityRef k -> Tick
entityRefTick (EntityRef uid) = uidTick uid

entityRefId :: (EntityIdClass (EntityIdFor k)) => EntityRef k -> EntityIdFor k
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
    UidPrefix 'Bout = "BOT"
    UidPrefix 'Contestant = "CNT"
    UidPrefix 'Dealer = "DLR"
    UidPrefix 'Hand = "HND"
    UidPrefix 'Player = "PLR"
    UidPrefix 'Round = "DRD"
    UidPrefix 'Shoe = "SHO"
    UidPrefix 'Table = "TBL"

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

instance ToJSON BoutId where toJSON (BoutId w) = toJSON w
instance FromJSON BoutId where parseJSON v = BoutId <$> parseJSON v
instance ToJSONKey BoutId
instance FromJSONKey BoutId

instance ToJSON ContestantId where toJSON (ContestantId w) = toJSON w
instance FromJSON ContestantId where parseJSON v = ContestantId <$> parseJSON v
instance ToJSONKey ContestantId
instance FromJSONKey ContestantId

instance ToJSON DealerId where toJSON (DealerId w) = toJSON w
instance FromJSON DealerId where parseJSON v = DealerId <$> parseJSON v
instance ToJSONKey DealerId
instance FromJSONKey DealerId

instance ToJSON HandId where toJSON (HandId w) = toJSON w
instance FromJSON HandId where parseJSON v = HandId <$> parseJSON v
instance ToJSONKey HandId
instance FromJSONKey HandId

instance ToJSON PlayerId where toJSON (PlayerId w) = toJSON w
instance FromJSON PlayerId where parseJSON v = PlayerId <$> parseJSON v
instance ToJSONKey PlayerId
instance FromJSONKey PlayerId

instance ToJSON RoundId where toJSON (RoundId w) = toJSON w
instance FromJSON RoundId where parseJSON v = RoundId <$> parseJSON v
instance ToJSONKey RoundId
instance FromJSONKey RoundId

instance ToJSON ShoeId where toJSON (ShoeId w) = toJSON w
instance FromJSON ShoeId where parseJSON v = ShoeId <$> parseJSON v
instance ToJSONKey ShoeId
instance FromJSONKey ShoeId

instance ToJSON TableId where toJSON (TableId w) = toJSON w
instance FromJSON TableId where parseJSON v = TableId <$> parseJSON v
instance ToJSONKey TableId
instance FromJSONKey TableId

instance forall k. (KnownSymbol (UidPrefix k), EntityIdClass (EntityIdFor k)) => ToJSON (EntityRef k) where
    toJSON (EntityRef uid) = toJSON uid

instance forall k. (KnownSymbol (UidPrefix k), EntityIdClass (EntityIdFor k)) => FromJSON (EntityRef k) where
    parseJSON v = EntityRef <$> parseJSON v

instance (EntityIdClass (EntityIdFor k), Eq (EntityIdFor k)) => Hashable (Uid k) where
    hashWithSalt salt (Uid (Tick tick, entityId)) =
        hashWithSalt (hashWithSalt salt tick) (toWord64 entityId)

instance (EntityIdClass (EntityIdFor k), Eq (EntityIdFor k)) => Hashable (EntityRef k) where
    hashWithSalt salt (EntityRef uid) = hashWithSalt salt uid

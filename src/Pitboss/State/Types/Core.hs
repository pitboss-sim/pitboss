{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Pitboss.State.Types.Core (
    EntityKind (..),
    EntityStatePart (..),
    EntityId (..),
    Uid (..),
    UidPrefix,
    EntityRef (..),
    Tick (..),
    IntentType (..),
    IntentDetails (..),
    EventType (..),
    EventDetails (..),
    OriginatingEntity (..),
    CardIx,
    CardState (..),
    PlayerSpotIx (..),
    PlayerSpotHandIx (..),
    uidTick,
    uidEntityId,
    entityRefTick,
    entityRefId,
    generateUid,
    displayUid,
    parseDisplayUid,
) where

import Data.Aeson (FromJSON (..), FromJSONKey, ToJSON (..), ToJSONKey, withText)
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
import Pitboss.Blackjack.Materia.Card (Card)
import Pitboss.Blackjack.Materia.Hand (SomeHand)
import Pitboss.FSM.Bout (SomeBoutFSM)
import Pitboss.State.Types.FiniteMap.BoundedEnum (BoundedEnum)
import System.Random (Random (..), RandomGen)

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

-- provenance

data IntentType
    = PlayerIntent
    | DealerIntent
    | TableIntent
    deriving (Eq, Show, Generic)

data IntentDetails
    = PlayerHitIntent
    | PlayerStandIntent
    | PlayerDoubleIntent
    | PlayerSplitIntent
    | PlayerSurrenderIntent
    | DealerHitIntent
    | DealerStandIntent
    | TableDealCardIntent (EntityId 'PlayerHand)
    | TableSettleBoutIntent (EntityId 'Bout)
    deriving (Eq, Show, Generic)

data OriginatingEntity
    = FromPlayer (EntityId 'Player)
    | FromDealer (EntityId 'Dealer)
    | FromTable (EntityId 'Table)
    deriving (Eq, Show, Generic)

instance ToJSON IntentType
instance FromJSON IntentType
instance ToJSON IntentDetails
instance FromJSON IntentDetails
instance ToJSON OriginatingEntity
instance FromJSON OriginatingEntity

data EventType
    = CardDealt
    | HandScored
    | BoutTransitioned
    | IntentValidated
    | IntentRejected
    deriving (Eq, Show, Generic)

instance ToJSON EventType
instance FromJSON EventType

data EventDetails
    = CardDealtDetails Card (EntityId 'PlayerHand)
    | HandScoredDetails SomeHand Int
    | BoutTransitionedDetails SomeBoutFSM SomeBoutFSM
    | IntentValidatedDetails (EntityId 'Intent)
    | IntentRejectedDetails (EntityId 'Intent) String
    deriving (Eq, Show, Generic)

instance ToJSON EventDetails
instance FromJSON EventDetails

-- card state minutiae

type CardIx = Int

data CardState
    = InHand
    | InDiscard
    | Burned
    deriving (Eq, Show, Generic)

instance ToJSON CardState
instance FromJSON CardState

-- player spot minutiae

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

-- time

newtype Tick = Tick Word64
    deriving (Eq, Ord, Show, Hashable, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

-- uid

newtype EntityId (k :: EntityKind) = EntityId Word64
    deriving (Eq, Ord, Show, Generic)

newtype Uid (k :: EntityKind) = Uid (Tick, EntityId k)
    deriving (Eq, Ord, Show, Generic)

newtype EntityRef (k :: EntityKind) = EntityRef (Uid k)
    deriving (Eq, Ord, Show, Generic)

-- Helper functions
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
        tsPart = showPaddedBase32 tickInt 6
        rndPart = showPaddedBase32 (fromIntegral entropy) 8
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
     in padLeft '0' width result

isBase32Char :: Char -> Bool
isBase32Char c = toUpper c `elem` ("0123456789ABCDEFGHJKMNPQRSTVWXYZ" :: String)

decodeBase32Char :: Char -> Maybe Int
decodeBase32Char = (`Map.lookup` base32Map) . toUpper
  where
    base32Map :: Map Char Int
    base32Map = Map.fromList $ zip "0123456789ABCDEFGHJKMNPQRSTVWXYZ" [0 .. 31]

type family UidPrefix (k :: EntityKind) :: Symbol where
    UidPrefix 'Intent = "INT"
    UidPrefix 'Event = "EVT"
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

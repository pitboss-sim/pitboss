{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Pitboss.State.Delta.Types (
    DeltaSemantics (..),
    SomeDelta (..),
    CausalHistory (..),
    Delta (..),
    extractCausalHistory,
    extractCausalIntent,
    extractCausalEvent,
) where

import Data.Aeson (
    FromJSON (..),
    ToJSON (..),
    object,
    withObject,
    (.:),
    (.=),
 )
import Data.Aeson.Types (Parser)
import Data.Map.Strict (Map)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Pitboss.Blackjack
import Pitboss.FSM
import Pitboss.State.Types.Core
import Pitboss.State.Types.FiniteMap.Occupancy

data SomeDelta k where
    AttrsDelta :: CausalHistory -> Delta k ('PartialUpdate 'Attrs) -> SomeDelta k
    ModesDelta :: CausalHistory -> Delta k ('PartialUpdate 'Modes) -> SomeDelta k
    RelsDelta :: CausalHistory -> Delta k ('PartialUpdate 'Rels) -> SomeDelta k
    BoundaryDelta :: CausalHistory -> Delta k 'TransactionBoundary -> SomeDelta k

instance
    ( Eq (Delta k ('PartialUpdate 'Attrs))
    , Eq (Delta k ('PartialUpdate 'Modes))
    , Eq (Delta k ('PartialUpdate 'Rels))
    , Eq (Delta k 'TransactionBoundary)
    ) =>
    Eq (SomeDelta k)
    where
    (AttrsDelta h1 d1) == (AttrsDelta h2 d2) = h1 == h2 && d1 == d2
    (ModesDelta h1 d1) == (ModesDelta h2 d2) = h1 == h2 && d1 == d2
    (RelsDelta h1 d1) == (RelsDelta h2 d2) = h1 == h2 && d1 == d2
    (BoundaryDelta h1 d1) == (BoundaryDelta h2 d2) = h1 == h2 && d1 == d2
    _ == _ = False

data instance Delta k 'TransactionBoundary = DTransactionBoundary
    deriving (Eq, Show, Generic)

data DeltaSemantics
    = TransactionBoundary
    | PartialUpdate EntityStatePart

data CausalHistory = CausalHistory
    { causalIntent :: Maybe (EntityId 'Intent)
    , causalEvent :: Maybe (EntityId 'Event)
    }
    deriving (Eq, Show, Generic)

instance ToJSON CausalHistory
instance FromJSON CausalHistory

data family Delta (k :: EntityKind) (s :: DeltaSemantics)

extractCausalHistory :: SomeDelta k -> CausalHistory
extractCausalHistory (AttrsDelta causal _) = causal
extractCausalHistory (ModesDelta causal _) = causal
extractCausalHistory (RelsDelta causal _) = causal
extractCausalHistory (BoundaryDelta causal _) = causal

extractCausalIntent :: SomeDelta k -> Maybe (EntityId 'Intent)
extractCausalIntent = causalIntent . extractCausalHistory

extractCausalEvent :: SomeDelta k -> Maybe (EntityId 'Event)
extractCausalEvent = causalEvent . extractCausalHistory

-- DIntent
data instance Delta 'Intent ('PartialUpdate 'Attrs)
    = DIntentSetType IntentType IntentType
    | DIntentSetKind IntentKind IntentKind
    | DIntentSetTimestamp Tick Tick
    | DIntentSetDescription String String
    deriving (Eq, Show, Generic)

data instance Delta 'Intent ('PartialUpdate 'Modes)
    deriving (Eq, Show, Generic)

data instance Delta 'Intent ('PartialUpdate 'Rels)
    = DIntentSetOriginatingEntity OriginatingEntity OriginatingEntity
    | DIntentSetTargetBout (Maybe (EntityId 'Bout)) (Maybe (EntityId 'Bout))
    deriving (Eq, Show, Generic)

-- DEvent
data instance Delta 'Event ('PartialUpdate 'Attrs)
    deriving (Eq, Show, Generic)

data instance Delta 'Event ('PartialUpdate 'Modes)
    deriving (Eq, Show, Generic)

data instance Delta 'Event ('PartialUpdate 'Rels)
    deriving (Eq, Show, Generic)

-- DBout
data instance Delta 'Bout ('PartialUpdate 'Attrs)
    = DBoutSetOutcome (Maybe DetailedOutcome) (Maybe DetailedOutcome)
    deriving (Eq, Show, Generic)

data instance Delta 'Bout ('PartialUpdate 'Modes)
    = DBoutSetFSM SomeBoutFSM SomeBoutFSM
    deriving (Eq, Show, Generic)

data instance Delta 'Bout ('PartialUpdate 'Rels)
    = DBoutSetPlayerHand (EntityId 'PlayerHand) (EntityId 'PlayerHand)
    | DBoutSetDealerHand (EntityId 'DealerHand) (EntityId 'DealerHand)
    | DBoutSetTableShoe (EntityId 'TableShoe) (EntityId 'TableShoe)
    | DBoutSetTable (EntityId 'Table) (EntityId 'Table)
    | DBoutSetDealerRound (EntityId 'DealerRound) (EntityId 'DealerRound)
    deriving (Eq, Show, Generic)

-- DDealer
data instance Delta 'Dealer ('PartialUpdate 'Attrs)
    = DDealerSetName String String
    deriving (Eq, Show, Generic)

data instance Delta 'Dealer ('PartialUpdate 'Modes)
    = DDealerSetTableFSM SomeDealerTableFSM SomeDealerTableFSM
    | DDealerSetRoundFSM DealerRoundFSM DealerRoundFSM
    | DDealerSetHandFSM SomeDealerHandFSM SomeDealerHandFSM
    deriving (Eq, Show, Generic)

data instance Delta 'Dealer ('PartialUpdate 'Rels)
    = DDealerSetActiveTable (Maybe (EntityId 'Table)) (Maybe (EntityId 'Table))
    | DDealerSetActiveRound (Maybe (EntityId 'DealerRound)) (Maybe (EntityId 'DealerRound))
    | DDealerSetActiveHand (Maybe (EntityId 'DealerHand)) (Maybe (EntityId 'DealerHand))
    deriving (Eq, Show, Generic)

-- DDealerHand
data instance Delta 'DealerHand ('PartialUpdate 'Attrs)
    = DDealerHandSetHand SomeHand SomeHand
    deriving (Eq, Show, Generic)

data instance Delta 'DealerHand ('PartialUpdate 'Modes)
    = DDealerHandSetFSM SomeDealerHandFSM SomeDealerHandFSM
    deriving (Eq, Show, Generic)

data instance Delta 'DealerHand ('PartialUpdate 'Rels)
    = DDealerHandSetRound (EntityId 'DealerRound) (EntityId 'DealerRound)
    | DDealerHandSetDealer (EntityId 'Dealer) (EntityId 'Dealer)
    deriving (Eq, Show, Generic)

-- DDealerRound
data instance Delta 'DealerRound ('PartialUpdate 'Attrs)
    = DDealerRoundSetNumber Int Int
    deriving (Eq, Show, Generic)

data instance Delta 'DealerRound ('PartialUpdate 'Modes)
    deriving (Eq, Show, Generic)

data instance Delta 'DealerRound ('PartialUpdate 'Rels)
    = DDealerRoundSetTableShoe (EntityId 'TableShoe) (EntityId 'TableShoe)
    deriving (Eq, Show, Generic)

-- DPlayer
data instance Delta 'Player ('PartialUpdate 'Attrs)
    = DPlayerSetName String String
    | DPlayerSetBankroll Chips Chips
    deriving (Eq, Show, Generic)

data instance Delta 'Player ('PartialUpdate 'Modes)
    = DPlayerSetTable (Maybe SomePlayerTableFSM) (Maybe SomePlayerTableFSM)
    | DPlayerSetSpot (Maybe SomePlayerSpotFSM) (Maybe SomePlayerSpotFSM)
    | DPlayerSetHand (Maybe SomePlayerHandFSM) (Maybe SomePlayerHandFSM)
    deriving (Eq, Show, Generic)

data instance Delta 'Player ('PartialUpdate 'Rels)
    deriving (Eq, Show, Generic)

-- DPlayerHand
data instance Delta 'PlayerHand ('PartialUpdate 'Attrs)
    = DPlayerHandSetPlayerHandIx Int Int
    | DPlayerHandSetSplitDepth Int Int
    | DPlayerHandSetHand SomeHand SomeHand
    deriving (Eq, Show, Generic)

data instance Delta 'PlayerHand ('PartialUpdate 'Modes)
    = DPlayerHandSetPlayerHandFSM SomePlayerHandFSM SomePlayerHandFSM
    deriving (Eq, Show, Generic)

data instance Delta 'PlayerHand ('PartialUpdate 'Rels)
    = DPlayerHandSetPlayerSpot (EntityId 'PlayerSpot) (EntityId 'PlayerSpot)
    deriving (Eq, Show, Generic)

-- DPlayerSpot
data instance Delta 'PlayerSpot ('PartialUpdate 'Attrs)
    = DPlayerSpotSetWager Chips Chips
    deriving (Eq, Show, Generic)

data instance Delta 'PlayerSpot ('PartialUpdate 'Modes)
    = DPlayerSpotSetFSM SomePlayerSpotFSM SomePlayerSpotFSM
    deriving (Eq, Show, Generic)

data instance Delta 'PlayerSpot ('PartialUpdate 'Rels)
    = DPlayerSpotSetPlayer (EntityId 'Player) (EntityId 'Player)
    | DPlayerSpotSetRound (EntityId 'DealerRound) (EntityId 'DealerRound)
    | DPlayerSpotSetHandOccupancy
        (PlayerSpotHandIx, Occupancy (EntityId 'PlayerHand))
        (PlayerSpotHandIx, Occupancy (EntityId 'PlayerHand))
    deriving (Eq, Show, Generic)

-- DTable
data instance Delta 'Table ('PartialUpdate 'Attrs)
    = DTableSetName String String
    | DTableSetOffering Offering Offering
    deriving (Eq, Show, Generic)

data instance Delta 'Table ('PartialUpdate 'Modes)
    deriving (Eq, Show, Generic)

data instance Delta 'Table ('PartialUpdate 'Rels)
    = DTableSetDealer (Maybe (EntityId 'Dealer)) (Maybe (EntityId 'Dealer))
    deriving (Eq, Show, Generic)

-- DTableShoe
data instance Delta 'TableShoe ('PartialUpdate 'Attrs)
    = DTableShoeSetCardStateMap (Map CardIx CardState) (Map CardIx CardState)
    | DTableShoeSetCardFate CardIx CardState CardState
    deriving (Eq, Show, Generic)

data instance Delta 'TableShoe ('PartialUpdate 'Modes)
    deriving (Eq, Show, Generic)

data instance Delta 'TableShoe ('PartialUpdate 'Rels)
    = DTableShoeSetTable (EntityId 'Table) (EntityId 'Table)
    deriving (Eq, Show, Generic)

instance
    ( ToJSON (Delta k ('PartialUpdate 'Attrs))
    , ToJSON (Delta k ('PartialUpdate 'Modes))
    , ToJSON (Delta k ('PartialUpdate 'Rels))
    , ToJSON (Delta k 'TransactionBoundary)
    ) =>
    ToJSON (SomeDelta k)
    where
    toJSON (AttrsDelta history delta) =
        object ["type" .= ("AttrsDelta" :: T.Text), "history" .= history, "delta" .= delta]
    toJSON (ModesDelta history delta) =
        object ["type" .= ("ModesDelta" :: T.Text), "history" .= history, "delta" .= delta]
    toJSON (RelsDelta history delta) =
        object ["type" .= ("RelsDelta" :: T.Text), "history" .= history, "delta" .= delta]
    toJSON (BoundaryDelta history delta) =
        object ["type" .= ("BoundaryDelta" :: T.Text), "history" .= history, "delta" .= delta]

instance
    ( FromJSON (Delta k ('PartialUpdate 'Attrs))
    , FromJSON (Delta k ('PartialUpdate 'Modes))
    , FromJSON (Delta k ('PartialUpdate 'Rels))
    , FromJSON (Delta k 'TransactionBoundary)
    ) =>
    FromJSON (SomeDelta k)
    where
    parseJSON = withObject "SomeDelta" $ \v -> do
        typ <- v .: "type" :: Parser T.Text
        case typ of
            "AttrsDelta" -> AttrsDelta <$> v .: "history" <*> v .: "delta"
            "ModesDelta" -> ModesDelta <$> v .: "history" <*> v .: "delta"
            "RelsDelta" -> RelsDelta <$> v .: "history" <*> v .: "delta"
            "BoundaryDelta" -> BoundaryDelta <$> v .: "history" <*> v .: "delta"
            _ -> fail $ "Unknown type: " ++ T.unpack typ

instance ToJSON (Delta 'Intent 'TransactionBoundary)
instance FromJSON (Delta 'Intent 'TransactionBoundary)
instance ToJSON (Delta 'Intent ('PartialUpdate 'Attrs))
instance FromJSON (Delta 'Intent ('PartialUpdate 'Attrs))
instance ToJSON (Delta 'Intent ('PartialUpdate 'Modes))
instance FromJSON (Delta 'Intent ('PartialUpdate 'Modes))
instance ToJSON (Delta 'Intent ('PartialUpdate 'Rels))
instance FromJSON (Delta 'Intent ('PartialUpdate 'Rels))

instance ToJSON (Delta 'Event 'TransactionBoundary)
instance FromJSON (Delta 'Event 'TransactionBoundary)
instance ToJSON (Delta 'Event ('PartialUpdate 'Attrs))
instance FromJSON (Delta 'Event ('PartialUpdate 'Attrs))
instance ToJSON (Delta 'Event ('PartialUpdate 'Modes))
instance FromJSON (Delta 'Event ('PartialUpdate 'Modes))
instance ToJSON (Delta 'Event ('PartialUpdate 'Rels))
instance FromJSON (Delta 'Event ('PartialUpdate 'Rels))

instance ToJSON (Delta 'Bout 'TransactionBoundary)
instance FromJSON (Delta 'Bout 'TransactionBoundary)
instance ToJSON (Delta 'Bout ('PartialUpdate 'Attrs))
instance FromJSON (Delta 'Bout ('PartialUpdate 'Attrs))
instance ToJSON (Delta 'Bout ('PartialUpdate 'Modes))
instance FromJSON (Delta 'Bout ('PartialUpdate 'Modes))
instance ToJSON (Delta 'Bout ('PartialUpdate 'Rels))
instance FromJSON (Delta 'Bout ('PartialUpdate 'Rels))

instance ToJSON (Delta 'Dealer 'TransactionBoundary)
instance FromJSON (Delta 'Dealer 'TransactionBoundary)
instance ToJSON (Delta 'Dealer ('PartialUpdate 'Attrs))
instance FromJSON (Delta 'Dealer ('PartialUpdate 'Attrs))
instance ToJSON (Delta 'Dealer ('PartialUpdate 'Modes))
instance FromJSON (Delta 'Dealer ('PartialUpdate 'Modes))
instance ToJSON (Delta 'Dealer ('PartialUpdate 'Rels))
instance FromJSON (Delta 'Dealer ('PartialUpdate 'Rels))

instance ToJSON (Delta 'DealerHand 'TransactionBoundary)
instance FromJSON (Delta 'DealerHand 'TransactionBoundary)
instance ToJSON (Delta 'DealerHand ('PartialUpdate 'Attrs))
instance FromJSON (Delta 'DealerHand ('PartialUpdate 'Attrs))
instance ToJSON (Delta 'DealerHand ('PartialUpdate 'Modes))
instance FromJSON (Delta 'DealerHand ('PartialUpdate 'Modes))
instance ToJSON (Delta 'DealerHand ('PartialUpdate 'Rels))
instance FromJSON (Delta 'DealerHand ('PartialUpdate 'Rels))

instance ToJSON (Delta 'DealerRound 'TransactionBoundary)
instance FromJSON (Delta 'DealerRound 'TransactionBoundary)
instance ToJSON (Delta 'DealerRound ('PartialUpdate 'Attrs))
instance FromJSON (Delta 'DealerRound ('PartialUpdate 'Attrs))
instance ToJSON (Delta 'DealerRound ('PartialUpdate 'Modes))
instance FromJSON (Delta 'DealerRound ('PartialUpdate 'Modes))
instance ToJSON (Delta 'DealerRound ('PartialUpdate 'Rels))
instance FromJSON (Delta 'DealerRound ('PartialUpdate 'Rels))

instance ToJSON (Delta 'Player 'TransactionBoundary)
instance FromJSON (Delta 'Player 'TransactionBoundary)
instance ToJSON (Delta 'Player ('PartialUpdate 'Attrs))
instance FromJSON (Delta 'Player ('PartialUpdate 'Attrs))
instance ToJSON (Delta 'Player ('PartialUpdate 'Modes))
instance FromJSON (Delta 'Player ('PartialUpdate 'Modes))
instance ToJSON (Delta 'Player ('PartialUpdate 'Rels))
instance FromJSON (Delta 'Player ('PartialUpdate 'Rels))

instance ToJSON (Delta 'PlayerHand 'TransactionBoundary)
instance FromJSON (Delta 'PlayerHand 'TransactionBoundary)
instance ToJSON (Delta 'PlayerHand ('PartialUpdate 'Attrs))
instance FromJSON (Delta 'PlayerHand ('PartialUpdate 'Attrs))
instance ToJSON (Delta 'PlayerHand ('PartialUpdate 'Modes))
instance FromJSON (Delta 'PlayerHand ('PartialUpdate 'Modes))
instance ToJSON (Delta 'PlayerHand ('PartialUpdate 'Rels))
instance FromJSON (Delta 'PlayerHand ('PartialUpdate 'Rels))

instance ToJSON (Delta 'PlayerSpot 'TransactionBoundary)
instance FromJSON (Delta 'PlayerSpot 'TransactionBoundary)
instance ToJSON (Delta 'PlayerSpot ('PartialUpdate 'Attrs))
instance FromJSON (Delta 'PlayerSpot ('PartialUpdate 'Attrs))
instance ToJSON (Delta 'PlayerSpot ('PartialUpdate 'Modes))
instance FromJSON (Delta 'PlayerSpot ('PartialUpdate 'Modes))
instance ToJSON (Delta 'PlayerSpot ('PartialUpdate 'Rels))
instance FromJSON (Delta 'PlayerSpot ('PartialUpdate 'Rels))

instance ToJSON (Delta 'Table 'TransactionBoundary)
instance FromJSON (Delta 'Table 'TransactionBoundary)
instance ToJSON (Delta 'Table ('PartialUpdate 'Attrs))
instance FromJSON (Delta 'Table ('PartialUpdate 'Attrs))
instance ToJSON (Delta 'Table ('PartialUpdate 'Modes))
instance FromJSON (Delta 'Table ('PartialUpdate 'Modes))
instance ToJSON (Delta 'Table ('PartialUpdate 'Rels))
instance FromJSON (Delta 'Table ('PartialUpdate 'Rels))

instance ToJSON (Delta 'TableShoe 'TransactionBoundary)
instance FromJSON (Delta 'TableShoe 'TransactionBoundary)
instance ToJSON (Delta 'TableShoe ('PartialUpdate 'Attrs))
instance FromJSON (Delta 'TableShoe ('PartialUpdate 'Attrs))
instance ToJSON (Delta 'TableShoe ('PartialUpdate 'Modes))
instance FromJSON (Delta 'TableShoe ('PartialUpdate 'Modes))
instance ToJSON (Delta 'TableShoe ('PartialUpdate 'Rels))
instance FromJSON (Delta 'TableShoe ('PartialUpdate 'Rels))

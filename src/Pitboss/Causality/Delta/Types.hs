{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Pitboss.Causality.Delta.Types where

import Data.Aeson.Types
import Data.Map.Strict (Map)
import Data.Text qualified as T
import GHC.Generics (Generic (..))
import Pitboss.Blackjack hiding (Hand)
import Pitboss.Causality.Entity.Types
import Pitboss.Causality.Types.Core
import Pitboss.Causality.Types.FiniteMap
import Pitboss.FSM

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

data family Delta (k :: EntityKind) (s :: DeltaSemantics)

extractCausalHistory :: SomeDelta k -> CausalHistory
extractCausalHistory (AttrsDelta causal _) = causal
extractCausalHistory (ModesDelta causal _) = causal
extractCausalHistory (RelsDelta causal _) = causal
extractCausalHistory (BoundaryDelta causal _) = causal

data instance Delta 'Bout ('PartialUpdate 'Attrs)
    = DBoutSetPlayerHand SomeHand SomeHand
    | DBoutSetDealerHand SomeHand SomeHand
    | DBoutSetActiveHandIx HandIx HandIx
    | DBoutSetOutcome (Occupancy DetailedOutcome) (Occupancy DetailedOutcome)
    deriving (Eq, Show, Generic)

data instance Delta 'Bout ('PartialUpdate 'Modes)
    = DBoutSetBoutFSM SomeBoutFSM SomeBoutFSM
    | DBoutSetPlayerHandFSM SomePlayerHandFSM SomePlayerHandFSM
    | DBoutSetDealerHandFSM SomeDealerHandFSM SomeDealerHandFSM
    deriving (Eq, Show, Generic)

data instance Delta 'Bout ('PartialUpdate 'Rels)
    = DBoutSetPlayer PlayerId PlayerId
    | DBoutSetDealer DealerId DealerId
    | DBoutSetRound RoundId RoundId
    | DBoutSetTable TableId TableId
    | DBoutSetPlayerBoutEntry HandIx (Occupancy BoutId) (Occupancy BoutId)
    deriving (Eq, Show, Generic)

data instance Delta 'Dealer ('PartialUpdate 'Attrs)
    = DDealerSetName String String
    deriving (Eq, Show, Generic)

data instance Delta 'Dealer ('PartialUpdate 'Modes)
    = DDealerSetTableFSM SomeDealerTableFSM SomeDealerTableFSM
    | DDealerSetRoundFSM RoundFSM RoundFSM
    deriving (Eq, Show, Generic)

data instance Delta 'Dealer ('PartialUpdate 'Rels)
    = DDealerSetActiveRound (Occupancy RoundId) (Occupancy RoundId)
    | DDealerSetActiveTable (Occupancy TableId) (Occupancy TableId)
    deriving (Eq, Show, Generic)

data instance Delta 'Player ('PartialUpdate 'Attrs)
    = DPlayerSetName String String
    | DPlayerSetBankroll Chips Chips
    deriving (Eq, Show, Generic)

data instance Delta 'Player ('PartialUpdate 'Modes)
    = DPlayerSetPlayerFSM SomePlayerFSM SomePlayerFSM
    deriving (Eq, Show, Generic)

data instance Delta 'Player ('PartialUpdate 'Rels)
    = DPlayerSetActiveRound (Occupancy RoundId) (Occupancy RoundId)
    | DPlayerSetActiveTable (Occupancy TableId) (Occupancy TableId)
    deriving (Eq, Show, Generic)

data instance Delta 'Round ('PartialUpdate 'Attrs)
    = DRoundSetNumber Int Int
    | DRoundSetActiveSpotIx (Occupancy TableSpotIx) (Occupancy TableSpotIx)
    deriving (Eq, Show, Generic)

data instance Delta 'Round ('PartialUpdate 'Modes)
    deriving (Eq, Show, Generic)

data instance Delta 'Round ('PartialUpdate 'Rels)
    = DRoundSetPlayerEntry TableSpotIx (Occupancy BoutId) (Occupancy BoutId)
    | DRoundSetShoe ShoeId ShoeId
    | DRoundSetTable TableId TableId
    deriving (Eq, Show, Generic)

data instance Delta 'Shoe ('PartialUpdate 'Attrs)
    = DShoeSetCards [Card] [Card]
    | DShoeSetCardStateMap (Map CardIx CardState) (Map CardIx CardState)
    | DShoeSetCardFate CardIx CardState CardState
    deriving (Eq, Show, Generic)

data instance Delta 'Shoe ('PartialUpdate 'Modes)
    deriving (Eq, Show, Generic)

data instance Delta 'Shoe ('PartialUpdate 'Rels)
    = DShoeSetTable TableId TableId
    deriving (Eq, Show, Generic)

data instance Delta 'Table ('PartialUpdate 'Attrs)
    = DTableSetName String String
    | DTableSetOffering Offering Offering
    | DTableSetSeatEntry TableSpotIx (Occupancy TableSeat) (Occupancy TableSeat)
    deriving (Eq, Show, Generic)

data instance Delta 'Table ('PartialUpdate 'Modes)
    = DTableSetFSM SomeTableFSM SomeTableFSM
    deriving (Eq, Show, Generic)

data instance Delta 'Table ('PartialUpdate 'Rels)
    = DTableSetActiveDealer (Occupancy DealerId) (Occupancy DealerId)
    | DTableSetActiveRound (Occupancy RoundId) (Occupancy RoundId)
    deriving (Eq, Show, Generic)

instance
    ( Show (Delta k ('PartialUpdate 'Attrs))
    , Show (Delta k ('PartialUpdate 'Modes))
    , Show (Delta k ('PartialUpdate 'Rels))
    , Show (Delta k 'TransactionBoundary)
    , Show CausalHistory
    ) =>
    Show (SomeDelta k)
    where
    show (AttrsDelta history delta) =
        "AttrsDelta " ++ show history ++ " (" ++ show delta ++ ")"
    show (ModesDelta history delta) =
        "ModesDelta " ++ show history ++ " (" ++ show delta ++ ")"
    show (RelsDelta history delta) =
        "RelsDelta " ++ show history ++ " (" ++ show delta ++ ")"
    show (BoundaryDelta history delta) =
        "BoundaryDelta " ++ show history ++ " (" ++ show delta ++ ")"

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

instance (Generic (Delta k s), GToJSON Zero (Rep (Delta k s))) => ToJSON (Delta k s) where
    toJSON = genericToJSON defaultOptions

instance (Generic (Delta k s), GFromJSON Zero (Rep (Delta k s))) => FromJSON (Delta k s) where
    parseJSON = genericParseJSON defaultOptions

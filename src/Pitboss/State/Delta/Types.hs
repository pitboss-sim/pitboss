{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Pitboss.State.Delta.Types (
    SomeDelta (..),
    Delta (..),
) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Map.Strict (Map)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Chips (Chips)
import Pitboss.Blackjack.Hand (SomeHand)
import Pitboss.Blackjack.Offering qualified as O
import Pitboss.FSM.DealerHand
import Pitboss.FSM.DealerRound
import Pitboss.FSM.DealerTable
import Pitboss.FSM.PlayerHand
import Pitboss.FSM.PlayerSpot (SomePlayerSpotFSM)
import Pitboss.FSM.PlayerTable
import Pitboss.State.Entity.Types
import Pitboss.State.Types.FiniteMap.Occupancy

data family Delta (k :: EntityKind) (s :: DeltaSemantics)

data SomeDelta k where
    AttrsUpdate :: Delta k ('PartialUpdate 'Attrs) -> SomeDelta k
    ModesUpdate :: Delta k ('PartialUpdate 'Modes) -> SomeDelta k
    RelsUpdate :: Delta k ('PartialUpdate 'Rels) -> SomeDelta k
    Boundary :: Delta k 'TransactionBoundary -> SomeDelta k

data instance Delta k 'TransactionBoundary = DTransactionBoundary
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

instance ToJSON (Delta 'Dealer 'TransactionBoundary)
instance FromJSON (Delta 'Dealer 'TransactionBoundary)

instance ToJSON (Delta 'Dealer ('PartialUpdate 'Attrs))
instance FromJSON (Delta 'Dealer ('PartialUpdate 'Attrs))

instance ToJSON (Delta 'Dealer ('PartialUpdate 'Modes))
instance FromJSON (Delta 'Dealer ('PartialUpdate 'Modes))

instance ToJSON (Delta 'Dealer ('PartialUpdate 'Rels))
instance FromJSON (Delta 'Dealer ('PartialUpdate 'Rels))

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

instance ToJSON (Delta 'DealerHand 'TransactionBoundary)
instance FromJSON (Delta 'DealerHand 'TransactionBoundary)

instance ToJSON (Delta 'DealerHand ('PartialUpdate 'Attrs))
instance FromJSON (Delta 'DealerHand ('PartialUpdate 'Attrs))

instance ToJSON (Delta 'DealerHand ('PartialUpdate 'Modes))
instance FromJSON (Delta 'DealerHand ('PartialUpdate 'Modes))

instance ToJSON (Delta 'DealerHand ('PartialUpdate 'Rels))
instance FromJSON (Delta 'DealerHand ('PartialUpdate 'Rels))

-- DDealerRound

data instance Delta 'DealerRound ('PartialUpdate 'Attrs)
    = DDealerRoundSetNumber Int Int
    deriving (Eq, Show, Generic)

data instance Delta 'DealerRound ('PartialUpdate 'Modes)
    deriving (Eq, Show, Generic)

data instance Delta 'DealerRound ('PartialUpdate 'Rels)
    = DDealerRoundSetTableShoe (EntityId 'TableShoe) (EntityId 'TableShoe)
    deriving (Eq, Show, Generic)

instance ToJSON (Delta 'DealerRound 'TransactionBoundary)
instance FromJSON (Delta 'DealerRound 'TransactionBoundary)

instance ToJSON (Delta 'DealerRound ('PartialUpdate 'Attrs))
instance FromJSON (Delta 'DealerRound ('PartialUpdate 'Attrs))

instance ToJSON (Delta 'DealerRound ('PartialUpdate 'Modes))
instance FromJSON (Delta 'DealerRound ('PartialUpdate 'Modes))

instance ToJSON (Delta 'DealerRound ('PartialUpdate 'Rels))
instance FromJSON (Delta 'DealerRound ('PartialUpdate 'Rels))

-- DOffering

data instance Delta 'Offering ('PartialUpdate 'Attrs)
    = DOfferingSetOffering O.Offering O.Offering
    deriving (Eq, Show, Generic)

data instance Delta 'Offering ('PartialUpdate 'Modes) = DOfferingModes {}
    deriving (Eq, Show, Generic)

data instance Delta 'Offering ('PartialUpdate 'Rels) = DOfferingRels {}
    deriving (Eq, Show, Generic)

instance ToJSON (Delta 'Offering 'TransactionBoundary)
instance FromJSON (Delta 'Offering 'TransactionBoundary)

instance ToJSON (Delta 'Offering ('PartialUpdate 'Attrs))
instance FromJSON (Delta 'Offering ('PartialUpdate 'Attrs))

instance ToJSON (Delta 'Offering ('PartialUpdate 'Modes))
instance FromJSON (Delta 'Offering ('PartialUpdate 'Modes))

instance ToJSON (Delta 'Offering ('PartialUpdate 'Rels))
instance FromJSON (Delta 'Offering ('PartialUpdate 'Rels))

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

instance ToJSON (Delta 'Player 'TransactionBoundary)
instance FromJSON (Delta 'Player 'TransactionBoundary)

instance ToJSON (Delta 'Player ('PartialUpdate 'Attrs))
instance FromJSON (Delta 'Player ('PartialUpdate 'Attrs))

instance ToJSON (Delta 'Player ('PartialUpdate 'Modes))
instance FromJSON (Delta 'Player ('PartialUpdate 'Modes))

instance ToJSON (Delta 'Player ('PartialUpdate 'Rels))
instance FromJSON (Delta 'Player ('PartialUpdate 'Rels))

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

instance ToJSON (Delta 'PlayerHand 'TransactionBoundary)
instance FromJSON (Delta 'PlayerHand 'TransactionBoundary)

instance ToJSON (Delta 'PlayerHand ('PartialUpdate 'Attrs))
instance FromJSON (Delta 'PlayerHand ('PartialUpdate 'Attrs))

instance ToJSON (Delta 'PlayerHand ('PartialUpdate 'Modes))
instance FromJSON (Delta 'PlayerHand ('PartialUpdate 'Modes))

instance ToJSON (Delta 'PlayerHand ('PartialUpdate 'Rels))
instance FromJSON (Delta 'PlayerHand ('PartialUpdate 'Rels))

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

instance ToJSON (Delta 'PlayerSpot 'TransactionBoundary)
instance FromJSON (Delta 'PlayerSpot 'TransactionBoundary)

instance ToJSON (Delta 'PlayerSpot ('PartialUpdate 'Attrs))
instance FromJSON (Delta 'PlayerSpot ('PartialUpdate 'Attrs))

instance ToJSON (Delta 'PlayerSpot ('PartialUpdate 'Modes))
instance FromJSON (Delta 'PlayerSpot ('PartialUpdate 'Modes))

instance ToJSON (Delta 'PlayerSpot ('PartialUpdate 'Rels))
instance FromJSON (Delta 'PlayerSpot ('PartialUpdate 'Rels))

-- DTable

data instance Delta 'Table ('PartialUpdate 'Attrs)
    = DTableSetName String String
    | DTableSetMinBet Chips Chips
    | DTableSetOffering (EntityId 'Offering) (EntityId 'Offering)
    deriving (Eq, Show, Generic)

data instance Delta 'Table ('PartialUpdate 'Modes)
    deriving (Eq, Show, Generic)

data instance Delta 'Table ('PartialUpdate 'Rels)
    = DTableSetDealer (Maybe (EntityId 'Dealer)) (Maybe (EntityId 'Dealer))
    deriving (Eq, Show, Generic)

instance ToJSON (Delta 'Table 'TransactionBoundary)
instance FromJSON (Delta 'Table 'TransactionBoundary)

instance ToJSON (Delta 'Table ('PartialUpdate 'Attrs))
instance FromJSON (Delta 'Table ('PartialUpdate 'Attrs))

instance ToJSON (Delta 'Table ('PartialUpdate 'Modes))
instance FromJSON (Delta 'Table ('PartialUpdate 'Modes))

instance ToJSON (Delta 'Table ('PartialUpdate 'Rels))
instance FromJSON (Delta 'Table ('PartialUpdate 'Rels))

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

instance ToJSON (Delta 'TableShoe 'TransactionBoundary)
instance FromJSON (Delta 'TableShoe 'TransactionBoundary)

instance ToJSON (Delta 'TableShoe ('PartialUpdate 'Attrs))
instance FromJSON (Delta 'TableShoe ('PartialUpdate 'Attrs))

instance ToJSON (Delta 'TableShoe ('PartialUpdate 'Modes))
instance FromJSON (Delta 'TableShoe ('PartialUpdate 'Modes))

instance ToJSON (Delta 'TableShoe ('PartialUpdate 'Rels))
instance FromJSON (Delta 'TableShoe ('PartialUpdate 'Rels))

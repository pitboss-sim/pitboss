{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Pitboss.State.Entity.Types (
    EntityKind (..),
    EntityId (..),
    EntityState (..),
    DeltaSemantics (..),
    EntityStatePart (..),
    Uid (..),
    CardIx,
    CardState,
    PlayerSpotHandIx,
) where

import Data.Aeson (
    FromJSON (..),
    FromJSONKey,
    ToJSON (..),
    ToJSONKey,
 )
import Data.Map.Strict
import GHC.Generics (Generic)
import Pitboss.Blackjack.Card (Card)
import Pitboss.Blackjack.Chips
import Pitboss.Blackjack.Offering qualified as O
import Pitboss.FSM.DealerHand
import Pitboss.FSM.DealerRound
import Pitboss.FSM.DealerTable
import Pitboss.FSM.PlayerHand
import Pitboss.FSM.PlayerSpot
import Pitboss.FSM.PlayerTable
import Pitboss.FSM.Table
import Pitboss.State.Types.Core
import Pitboss.State.Types.FiniteMap
import Pitboss.State.Types.FiniteMap.BoundedEnum
import Pitboss.State.Types.FiniteMap.Occupancy

data family EntityState (k :: EntityKind) (s :: DeltaSemantics)

-- EDealer

data instance EntityState 'Dealer 'TransactionBoundary = EDealer
    { _dAttrs :: EntityState 'Dealer (PartialUpdate 'Attrs)
    , _dModes :: EntityState 'Dealer (PartialUpdate 'Modes)
    , _dRels :: EntityState 'Dealer (PartialUpdate 'Rels)
    }
    deriving (Eq, Show, Generic)

data instance EntityState 'Dealer (PartialUpdate 'Attrs) = EDealerAttrs
    { _dAttrsName :: String
    }
    deriving (Eq, Show, Generic)

data instance EntityState 'Dealer (PartialUpdate 'Modes) = EDealerModes
    { _dModesDealerTable :: SomeDealerTableFSM
    , _dModesDealerRound :: DealerRoundFSM
    , _dModesDealerHand :: SomeDealerHandFSM
    }
    deriving (Eq, Show, Generic)

data instance EntityState 'Dealer (PartialUpdate 'Rels) = EDealerRels
    { _dRelsActiveTable :: Maybe (EntityId 'Table)
    , _dRelsActiveRound :: Maybe (EntityId 'DealerRound)
    , _dRelsActiveHand :: Maybe (EntityId 'DealerHand)
    }
    deriving (Eq, Show, Generic)

instance ToJSON (EntityState 'Dealer 'TransactionBoundary)
instance FromJSON (EntityState 'Dealer 'TransactionBoundary)

instance ToJSON (EntityState 'Dealer (PartialUpdate 'Attrs))
instance FromJSON (EntityState 'Dealer (PartialUpdate 'Attrs))

instance ToJSON (EntityState 'Dealer (PartialUpdate 'Modes))
instance FromJSON (EntityState 'Dealer (PartialUpdate 'Modes))

instance ToJSON (EntityState 'Dealer (PartialUpdate 'Rels))
instance FromJSON (EntityState 'Dealer (PartialUpdate 'Rels))

-- EDealerHand

data instance EntityState 'DealerHand 'TransactionBoundary = EDealerHand
    { _dhAttrs :: EntityState 'DealerHand (PartialUpdate 'Attrs)
    , _dhModes :: EntityState 'DealerHand (PartialUpdate 'Modes)
    , _dhRels :: EntityState 'DealerHand (PartialUpdate 'Rels)
    }
    deriving (Eq, Show, Generic)

data instance EntityState 'DealerHand (PartialUpdate 'Attrs) = EDealerHandAttrs
    { _dhAttrsHandCards :: [Card]
    }
    deriving (Eq, Show, Generic)

data instance EntityState 'DealerHand (PartialUpdate 'Modes) = EDealerHandModes
    { _dhModesDealerHand :: SomeDealerHandFSM
    }
    deriving (Eq, Show, Generic)

data instance EntityState 'DealerHand (PartialUpdate 'Rels) = EDealerHandRels
    { _dhRelsDealerRound :: EntityId 'DealerRound
    , _dhRelsDealer :: EntityId 'Dealer
    }
    deriving (Eq, Show, Generic)

instance ToJSON (EntityState 'DealerHand 'TransactionBoundary)
instance FromJSON (EntityState 'DealerHand 'TransactionBoundary)

instance ToJSON (EntityState 'DealerHand (PartialUpdate 'Attrs))
instance FromJSON (EntityState 'DealerHand (PartialUpdate 'Attrs))

instance ToJSON (EntityState 'DealerHand (PartialUpdate 'Modes))
instance FromJSON (EntityState 'DealerHand (PartialUpdate 'Modes))

instance ToJSON (EntityState 'DealerHand (PartialUpdate 'Rels))
instance FromJSON (EntityState 'DealerHand (PartialUpdate 'Rels))

-- EDealerRound

data instance EntityState 'DealerRound 'TransactionBoundary = EDealerRound
    { _drAttrs :: EntityState 'DealerRound (PartialUpdate 'Attrs)
    , _drModes :: EntityState 'DealerRound (PartialUpdate 'Modes)
    , _drRels :: EntityState 'DealerRound (PartialUpdate 'Rels)
    }
    deriving (Eq, Show, Generic)

data instance EntityState 'DealerRound (PartialUpdate 'Attrs) = EDealerRoundAttrs
    { _drAttrsNumber :: Int
    , _drAttrsIsActive :: Bool
    }
    deriving (Eq, Show, Generic)

data instance EntityState 'DealerRound (PartialUpdate 'Modes) = EDealerRoundModes
    deriving (Eq, Show, Generic)

data instance EntityState 'DealerRound (PartialUpdate 'Rels) = EDealerRoundRels
    { _drRelsTableShoeUsed :: EntityId 'TableShoe
    }
    deriving (Eq, Show, Generic)

instance ToJSON (EntityState 'DealerRound 'TransactionBoundary)
instance FromJSON (EntityState 'DealerRound 'TransactionBoundary)

instance ToJSON (EntityState 'DealerRound (PartialUpdate 'Attrs))
instance FromJSON (EntityState 'DealerRound (PartialUpdate 'Attrs))

instance ToJSON (EntityState 'DealerRound (PartialUpdate 'Modes))
instance FromJSON (EntityState 'DealerRound (PartialUpdate 'Modes))

instance ToJSON (EntityState 'DealerRound (PartialUpdate 'Rels))
instance FromJSON (EntityState 'DealerRound (PartialUpdate 'Rels))

-- EOffering

data instance EntityState 'Offering 'TransactionBoundary = EOffering
    { _oAttrs :: EntityState 'Offering (PartialUpdate 'Attrs)
    , _oModes :: EntityState 'Offering (PartialUpdate 'Modes)
    , _oRels :: EntityState 'Offering (PartialUpdate 'Rels)
    }
    deriving (Eq, Show, Generic)

data instance EntityState 'Offering (PartialUpdate 'Attrs) = EOfferingAttrs
    { _oAttrsOffering :: O.Offering
    }
    deriving (Eq, Show, Generic)

data instance EntityState 'Offering (PartialUpdate 'Modes) = EOfferingModes
    deriving (Eq, Show, Generic)

data instance EntityState 'Offering (PartialUpdate 'Rels) = EOfferingRels
    deriving (Eq, Show, Generic)

instance ToJSON (EntityState 'Offering 'TransactionBoundary)
instance FromJSON (EntityState 'Offering 'TransactionBoundary)

instance ToJSON (EntityState 'Offering (PartialUpdate 'Attrs))
instance FromJSON (EntityState 'Offering (PartialUpdate 'Attrs))

instance ToJSON (EntityState 'Offering (PartialUpdate 'Modes))
instance FromJSON (EntityState 'Offering (PartialUpdate 'Modes))

instance ToJSON (EntityState 'Offering (PartialUpdate 'Rels))
instance FromJSON (EntityState 'Offering (PartialUpdate 'Rels))

-- EPlayer

data instance EntityState 'Player 'TransactionBoundary = EPlayer
    { _pAttrs :: EntityState 'Player (PartialUpdate 'Attrs)
    , _pModes :: EntityState 'Player (PartialUpdate 'Modes)
    , _pRels :: EntityState 'Player (PartialUpdate 'Rels)
    }
    deriving (Eq, Show, Generic)

data instance EntityState 'Player (PartialUpdate 'Attrs) = EPlayerAttrs
    { _pAttrsName :: String
    , _pAttrsBankroll :: Chips
    }
    deriving (Eq, Show, Generic)

data instance EntityState 'Player (PartialUpdate 'Modes) = EPlayerModes
    { _pModesPlayerTable :: SomePlayerTableFSM
    , _pModesPlayerSpot :: SomePlayerSpotFSM
    , _pModesPlayerHand :: SomePlayerHandFSM
    }
    deriving (Eq, Show, Generic)

data instance EntityState 'Player (PartialUpdate 'Rels) = EPlayerRels
    deriving (Eq, Show, Generic)

instance ToJSON (EntityState 'Player 'TransactionBoundary)
instance FromJSON (EntityState 'Player 'TransactionBoundary)

instance ToJSON (EntityState 'Player (PartialUpdate 'Attrs))
instance FromJSON (EntityState 'Player (PartialUpdate 'Attrs))

instance ToJSON (EntityState 'Player (PartialUpdate 'Modes))
instance FromJSON (EntityState 'Player (PartialUpdate 'Modes))

instance ToJSON (EntityState 'Player (PartialUpdate 'Rels))
instance FromJSON (EntityState 'Player (PartialUpdate 'Rels))

-- EPlayerHand

data instance EntityState 'PlayerHand 'TransactionBoundary = EPlayerHand
    { _phAttrs :: EntityState 'PlayerHand (PartialUpdate 'Attrs)
    , _phModes :: EntityState 'PlayerHand (PartialUpdate 'Modes)
    , _phRels :: EntityState 'PlayerHand (PartialUpdate 'Rels)
    }
    deriving (Eq, Show, Generic)

data instance EntityState 'PlayerHand (PartialUpdate 'Attrs) = EPlayerHandAttrs
    { _phAttrsHandCards :: [Card]
    , _phAttrsOriginalBet :: Chips
    , _phAttrsSplitDepth :: Int
    , _phAttrsHandIx :: Int
    }
    deriving (Eq, Show, Generic)

data instance EntityState 'PlayerHand (PartialUpdate 'Modes) = EPlayerHandModes
    { _phFsm :: SomePlayerHandFSM
    }
    deriving (Eq, Show, Generic)

data instance EntityState 'PlayerHand (PartialUpdate 'Rels) = EPlayerHandRels
    { _phRelsBelongsToPlayerSpot :: EntityId 'PlayerSpot
    , _phRelsBelongsToDealerRound :: EntityId 'DealerRound
    , _phRelsOwnedByPlayer :: EntityId 'Player
    }
    deriving (Eq, Show, Generic)

instance ToJSON (EntityState 'PlayerHand 'TransactionBoundary)
instance FromJSON (EntityState 'PlayerHand 'TransactionBoundary)

instance ToJSON (EntityState 'PlayerHand (PartialUpdate 'Attrs))
instance FromJSON (EntityState 'PlayerHand (PartialUpdate 'Attrs))

instance ToJSON (EntityState 'PlayerHand (PartialUpdate 'Modes))
instance FromJSON (EntityState 'PlayerHand (PartialUpdate 'Modes))

instance ToJSON (EntityState 'PlayerHand (PartialUpdate 'Rels))
instance FromJSON (EntityState 'PlayerHand (PartialUpdate 'Rels))

data instance EntityState 'PlayerSpot 'TransactionBoundary = EPlayerSpot
    { _psAttrs :: EntityState 'PlayerSpot (PartialUpdate 'Attrs)
    , _psModes :: EntityState 'PlayerSpot (PartialUpdate 'Modes)
    , _psRels :: EntityState 'PlayerSpot (PartialUpdate 'Rels)
    }
    deriving (Eq, Show, Generic)

data instance EntityState 'PlayerSpot (PartialUpdate 'Attrs) = EPlayerSpotAttrs
    { _psAttrsSpotIndex :: PlayerSpotIx
    , _psAttrsWager :: Chips
    }
    deriving (Eq, Show, Generic)

data instance EntityState 'PlayerSpot (PartialUpdate 'Modes) = EPlayerSpotModes
    { _psModesPlayerSpot :: SomePlayerSpotFSM
    }
    deriving (Eq, Show, Generic)

data instance EntityState 'PlayerSpot (PartialUpdate 'Rels) = EPlayerSpotRels
    { _psEntityRelsPlayerId :: EntityId 'Player
    , _psEntityRelsRoundId :: EntityId 'DealerRound
    , _psRelsHandOccupancy :: FiniteMap PlayerSpotHandIx (Occupancy (EntityId 'PlayerHand))
    }
    deriving (Eq, Show, Generic)

instance ToJSON (EntityState 'PlayerSpot 'TransactionBoundary)
instance FromJSON (EntityState 'PlayerSpot 'TransactionBoundary)

instance ToJSON (EntityState 'PlayerSpot (PartialUpdate 'Attrs))
instance FromJSON (EntityState 'PlayerSpot (PartialUpdate 'Attrs))

instance ToJSON (EntityState 'PlayerSpot (PartialUpdate 'Modes))
instance FromJSON (EntityState 'PlayerSpot (PartialUpdate 'Modes))

instance ToJSON (EntityState 'PlayerSpot (PartialUpdate 'Rels))
instance FromJSON (EntityState 'PlayerSpot (PartialUpdate 'Rels))

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

-- ETable

data instance EntityState 'Table 'TransactionBoundary = ETable
    { _tAttrs :: EntityState 'Table (PartialUpdate 'Attrs)
    , _tModes :: EntityState 'Table (PartialUpdate 'Modes)
    , _tRels :: EntityState 'Table (PartialUpdate 'Rels)
    }
    deriving (Eq, Show, Generic)

data instance EntityState 'Table (PartialUpdate 'Attrs) = ETableAttrs
    { _tAttrsName :: String
    , _tAttrsCurrentRound :: Maybe (EntityId 'DealerRound)
    , _tAttrsOfferingUsed :: EntityId 'Offering
    , _tAttrsMinBet :: Chips
    }
    deriving (Eq, Show, Generic)

data instance EntityState 'Table (PartialUpdate 'Modes) = ETableModes
    { _tModesFSM :: SomeTableFSM
    }
    deriving (Eq, Show, Generic)

data instance EntityState 'Table (PartialUpdate 'Rels) = ETableRels
    { _tRelsManagedByDealer :: Maybe (EntityId 'Dealer)
    }
    deriving (Eq, Show, Generic)

instance ToJSON (EntityState 'Table 'TransactionBoundary)
instance FromJSON (EntityState 'Table 'TransactionBoundary)

instance ToJSON (EntityState 'Table (PartialUpdate 'Attrs))
instance FromJSON (EntityState 'Table (PartialUpdate 'Attrs))

instance ToJSON (EntityState 'Table (PartialUpdate 'Modes))
instance FromJSON (EntityState 'Table (PartialUpdate 'Modes))

instance ToJSON (EntityState 'Table (PartialUpdate 'Rels))
instance FromJSON (EntityState 'Table (PartialUpdate 'Rels))

-- ETableShoe

data instance EntityState 'TableShoe 'TransactionBoundary = ETableShoe
    { _tsAttrs :: EntityState 'TableShoe (PartialUpdate 'Attrs)
    , _tsModes :: EntityState 'TableShoe (PartialUpdate 'Modes)
    , _tsRels :: EntityState 'TableShoe (PartialUpdate 'Rels)
    }
    deriving (Eq, Show, Generic)

data instance EntityState 'TableShoe (PartialUpdate 'Attrs) = ETableShoeAttrs
    { _tsAttrsCards :: [Card]
    , _tsAttrsCardStates :: Map CardIx CardState
    }
    deriving (Eq, Show, Generic)

data instance EntityState 'TableShoe (PartialUpdate 'Modes) = ETableShoeModes
    deriving (Eq, Show, Generic)

data instance EntityState 'TableShoe (PartialUpdate 'Rels) = ETableShoeRels
    { _tsRelsTable :: EntityId 'Table
    }
    deriving (Eq, Show, Generic)

instance ToJSON (EntityState 'TableShoe 'TransactionBoundary)
instance FromJSON (EntityState 'TableShoe 'TransactionBoundary)

instance ToJSON (EntityState 'TableShoe (PartialUpdate 'Attrs))
instance FromJSON (EntityState 'TableShoe (PartialUpdate 'Attrs))

instance ToJSON (EntityState 'TableShoe (PartialUpdate 'Modes))
instance FromJSON (EntityState 'TableShoe (PartialUpdate 'Modes))

instance ToJSON (EntityState 'TableShoe (PartialUpdate 'Rels))
instance FromJSON (EntityState 'TableShoe (PartialUpdate 'Rels))

type CardIx = Int

data CardState
    = InHand
    | InDiscard
    | Burned
    deriving (Eq, Show, Generic)

instance ToJSON CardState
instance FromJSON CardState

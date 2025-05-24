{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Pitboss.State.Delta.Types (
    Delta (..),
) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Map.Strict (Map)
import GHC.Generics (Generic)
import Pitboss.Blackjack.Card (Card)
import Pitboss.Blackjack.Chips (Chips)
import Pitboss.Blackjack.Offering qualified as O
import Pitboss.FSM.DealerHand
import Pitboss.FSM.DealerRound
import Pitboss.FSM.DealerTable
import Pitboss.FSM.PlayerHand
import Pitboss.FSM.PlayerSpot (SomePlayerSpotFSM)
import Pitboss.FSM.PlayerTable
import Pitboss.State.Entity.Types
import Pitboss.State.Types.FiniteMap.Occupancy

data family Delta (k :: EntityKind) (s :: EntityStateSelector)

-- DDealer

data instance Delta 'Dealer 'Whole = DDealer
    { _dAttrs :: Delta 'Dealer (Part 'Attrs)
    , _dModes :: Delta 'Dealer (Part 'Modes)
    , _dRels :: Delta 'Dealer (Part 'Rels)
    }
    deriving (Eq, Show, Generic)

data instance Delta 'Dealer (Part 'Attrs)
    = DDealerSetName String String
    deriving (Eq, Show, Generic)

data instance Delta 'Dealer (Part 'Modes)
    = DDealerSetTableFSM SomeDealerTableFSM SomeDealerTableFSM
    | DDealerSetRoundFSM DealerRoundFSM DealerRoundFSM
    | DDealerSetHandFSM SomeDealerHandFSM SomeDealerHandFSM
    deriving (Eq, Show, Generic)

data instance Delta 'Dealer (Part 'Rels)
    = DDealerSetActiveTable (Maybe (EntityRef 'Table)) (Maybe (EntityRef 'Table))
    | DDealerSetActiveRound (Maybe (EntityRef 'DealerRound)) (Maybe (EntityRef 'DealerRound))
    | DDealerSetActiveHand (Maybe (EntityRef 'DealerHand)) (Maybe (EntityRef 'DealerHand))
    deriving (Eq, Show, Generic)

instance ToJSON (Delta 'Dealer 'Whole)
instance FromJSON (Delta 'Dealer 'Whole)

instance ToJSON (Delta 'Dealer (Part 'Attrs))
instance FromJSON (Delta 'Dealer (Part 'Attrs))

instance ToJSON (Delta 'Dealer (Part 'Modes))
instance FromJSON (Delta 'Dealer (Part 'Modes))

instance ToJSON (Delta 'Dealer (Part 'Rels))
instance FromJSON (Delta 'Dealer (Part 'Rels))

-- DDealerHand

data instance Delta 'DealerHand 'Whole = DDealerHand
    { _dhAttrs :: Delta 'DealerHand (Part 'Attrs)
    , _dhModes :: Delta 'DealerHand (Part 'Modes)
    , _dhRels :: Delta 'DealerHand (Part 'Rels)
    }
    deriving (Eq, Show, Generic)

data instance Delta 'DealerHand (Part 'Attrs)
    = DDealerHandPushCard Card [Card]
    | DDealerHandPopCard Card [Card]
    | DDealerHandSetCards [Card] [Card]
    deriving (Eq, Show, Generic)

data instance Delta 'DealerHand (Part 'Modes)
    = DDealerHandSetFSM SomeDealerHandFSM SomeDealerHandFSM
    deriving (Eq, Show, Generic)

data instance Delta 'DealerHand (Part 'Rels)
    = DDealerHandSetRound (EntityRef 'DealerRound) (EntityRef 'DealerRound)
    | DDealerHandSetDealer (EntityRef 'Dealer) (EntityRef 'Dealer)
    deriving (Eq, Show, Generic)

instance ToJSON (Delta 'DealerHand 'Whole)
instance FromJSON (Delta 'DealerHand 'Whole)

instance ToJSON (Delta 'DealerHand (Part 'Attrs))
instance FromJSON (Delta 'DealerHand (Part 'Attrs))

instance ToJSON (Delta 'DealerHand (Part 'Modes))
instance FromJSON (Delta 'DealerHand (Part 'Modes))

instance ToJSON (Delta 'DealerHand (Part 'Rels))
instance FromJSON (Delta 'DealerHand (Part 'Rels))

-- DDealerRound

data instance Delta 'DealerRound 'Whole = DDealerRound
    { _drAttrs :: Delta 'DealerRound (Part 'Attrs)
    , _drModes :: Delta 'DealerRound (Part 'Modes)
    , _drRels :: Delta 'DealerRound (Part 'Rels)
    }
    deriving (Eq, Show, Generic)

data instance Delta 'DealerRound (Part 'Attrs)
    = DDealerRoundSetNumber Int Int
    deriving (Eq, Show, Generic)

data instance Delta 'DealerRound (Part 'Modes)
    deriving (Eq, Show, Generic)

data instance Delta 'DealerRound (Part 'Rels)
    = DDealerRoundSetTableShoe (EntityRef 'TableShoe) (EntityRef 'TableShoe)
    deriving (Eq, Show, Generic)

instance ToJSON (Delta 'DealerRound 'Whole)
instance FromJSON (Delta 'DealerRound 'Whole)

instance ToJSON (Delta 'DealerRound (Part 'Attrs))
instance FromJSON (Delta 'DealerRound (Part 'Attrs))

instance ToJSON (Delta 'DealerRound (Part 'Modes))
instance FromJSON (Delta 'DealerRound (Part 'Modes))

instance ToJSON (Delta 'DealerRound (Part 'Rels))
instance FromJSON (Delta 'DealerRound (Part 'Rels))

-- DOffering

data instance Delta 'Offering 'Whole = DOffering
    { _oAttrs :: Delta 'Offering (Part 'Attrs)
    , _oModes :: Delta 'Offering (Part 'Modes)
    , _oRels :: Delta 'Offering (Part 'Rels)
    }
    deriving (Eq, Show, Generic)

data instance Delta 'Offering (Part 'Attrs)
    = DOfferingSetOffering O.Offering O.Offering
    deriving (Eq, Show, Generic)

data instance Delta 'Offering (Part 'Modes) = DOfferingModes {}
    deriving (Eq, Show, Generic)

data instance Delta 'Offering (Part 'Rels) = DOfferingRels {}
    deriving (Eq, Show, Generic)

instance ToJSON (Delta 'Offering 'Whole)
instance FromJSON (Delta 'Offering 'Whole)

instance ToJSON (Delta 'Offering (Part 'Attrs))
instance FromJSON (Delta 'Offering (Part 'Attrs))

instance ToJSON (Delta 'Offering (Part 'Modes))
instance FromJSON (Delta 'Offering (Part 'Modes))

instance ToJSON (Delta 'Offering (Part 'Rels))
instance FromJSON (Delta 'Offering (Part 'Rels))

-- DPlayer

data instance Delta 'Player 'Whole = DPlayer
    { _pAttrs :: Delta 'Player (Part 'Attrs)
    , _pModes :: Delta 'Player (Part 'Modes)
    , _pRels :: Delta 'Player (Part 'Rels)
    }
    deriving (Eq, Show, Generic)

data instance Delta 'Player (Part 'Attrs)
    = DPlayerSetName String String
    | DPlayerSetBankroll Chips Chips
    deriving (Eq, Show, Generic)

data instance Delta 'Player (Part 'Modes)
    = DPlayerSetTable (Maybe SomePlayerTableFSM) (Maybe SomePlayerTableFSM)
    | DPlayerSetSpot (Maybe SomePlayerSpotFSM) (Maybe SomePlayerSpotFSM)
    | DPlayerSetHand (Maybe SomePlayerHandFSM) (Maybe SomePlayerHandFSM)
    deriving (Eq, Show, Generic)

data instance Delta 'Player (Part 'Rels)
    deriving (Eq, Show, Generic)

instance ToJSON (Delta 'Player 'Whole)
instance FromJSON (Delta 'Player 'Whole)

instance ToJSON (Delta 'Player (Part 'Attrs))
instance FromJSON (Delta 'Player (Part 'Attrs))

instance ToJSON (Delta 'Player (Part 'Modes))
instance FromJSON (Delta 'Player (Part 'Modes))

instance ToJSON (Delta 'Player (Part 'Rels))
instance FromJSON (Delta 'Player (Part 'Rels))

-- DPlayerHand

data instance Delta 'PlayerHand 'Whole = DPlayerHand
    { _phAttrs :: Delta 'PlayerHand (Part 'Attrs)
    , _phModes :: Delta 'PlayerHand (Part 'Modes)
    , _phRels :: Delta 'PlayerHand (Part 'Rels)
    }
    deriving (Eq, Show, Generic)

data instance Delta 'PlayerHand (Part 'Attrs)
    = DPlayerHandSetPlayerHandIx Int Int
    | DPlayerHandSetSplitDepth Int Int
    | DPlayerHandPushCard Card [Card]
    | DPlayerHandPopCard Card [Card]
    | DPlayerHandSetCards [Card] [Card]
    deriving (Eq, Show, Generic)

data instance Delta 'PlayerHand (Part 'Modes)
    = DPlayerHandSetPlayerHandFSM SomePlayerHandFSM SomePlayerHandFSM
    deriving (Eq, Show, Generic)

data instance Delta 'PlayerHand (Part 'Rels)
    = DPlayerHandSetPlayerSpot (EntityRef 'PlayerSpot) (EntityRef 'PlayerSpot)
    deriving (Eq, Show, Generic)

instance ToJSON (Delta 'PlayerHand 'Whole)
instance FromJSON (Delta 'PlayerHand 'Whole)

instance ToJSON (Delta 'PlayerHand (Part 'Attrs))
instance FromJSON (Delta 'PlayerHand (Part 'Attrs))

instance ToJSON (Delta 'PlayerHand (Part 'Modes))
instance FromJSON (Delta 'PlayerHand (Part 'Modes))

instance ToJSON (Delta 'PlayerHand (Part 'Rels))
instance FromJSON (Delta 'PlayerHand (Part 'Rels))

-- DPlayerSpot

data instance Delta 'PlayerSpot 'Whole = DPlayerSpot
    { _psAttrs :: Delta 'PlayerSpot (Part 'Attrs)
    , _psModes :: Delta 'PlayerSpot (Part 'Modes)
    , _psRels :: Delta 'PlayerSpot (Part 'Rels)
    }
    deriving (Eq, Show, Generic)

data instance Delta 'PlayerSpot (Part 'Attrs)
    = DPlayerSpotSetWager Chips Chips
    deriving (Eq, Show, Generic)

data instance Delta 'PlayerSpot (Part 'Modes)
    = DPlayerSpotSetFSM SomePlayerSpotFSM SomePlayerSpotFSM
    deriving (Eq, Show, Generic)

data instance Delta 'PlayerSpot (Part 'Rels)
    = DPlayerSpotSetPlayer (EntityRef 'Player) (EntityRef 'Player)
    | DPlayerSpotSetRound (EntityRef 'DealerRound) (EntityRef 'DealerRound)
    | DPlayerSpotSetHandOccupancy
        (PlayerSpotHandIx, Occupancy (EntityRef 'PlayerHand))
        (PlayerSpotHandIx, Occupancy (EntityRef 'PlayerHand))
    deriving (Eq, Show, Generic)

instance ToJSON (Delta 'PlayerSpot 'Whole)
instance FromJSON (Delta 'PlayerSpot 'Whole)

instance ToJSON (Delta 'PlayerSpot (Part 'Attrs))
instance FromJSON (Delta 'PlayerSpot (Part 'Attrs))

instance ToJSON (Delta 'PlayerSpot (Part 'Modes))
instance FromJSON (Delta 'PlayerSpot (Part 'Modes))

instance ToJSON (Delta 'PlayerSpot (Part 'Rels))
instance FromJSON (Delta 'PlayerSpot (Part 'Rels))

-- DTable

data instance Delta 'Table 'Whole = DTable
    { _tAttrs :: Delta 'Table (Part 'Attrs)
    , _tModes :: Delta 'Table (Part 'Modes)
    , _tRels :: Delta 'Table (Part 'Rels)
    }
    deriving (Eq, Show, Generic)

data instance Delta 'Table (Part 'Attrs)
    = DTableSetName String String
    | DTableSetMinBet Chips Chips
    | DTableSetOffering (EntityRef 'Offering) (EntityRef 'Offering)
    deriving (Eq, Show, Generic)

data instance Delta 'Table (Part 'Modes)
    deriving (Eq, Show, Generic)

data instance Delta 'Table (Part 'Rels)
    = DTableSetDealer (Maybe (EntityRef 'Dealer)) (Maybe (EntityRef 'Dealer))
    deriving (Eq, Show, Generic)

instance ToJSON (Delta 'Table 'Whole)
instance FromJSON (Delta 'Table 'Whole)

instance ToJSON (Delta 'Table (Part 'Attrs))
instance FromJSON (Delta 'Table (Part 'Attrs))

instance ToJSON (Delta 'Table (Part 'Modes))
instance FromJSON (Delta 'Table (Part 'Modes))

instance ToJSON (Delta 'Table (Part 'Rels))
instance FromJSON (Delta 'Table (Part 'Rels))

-- DTableShoe

data instance Delta 'TableShoe 'Whole = DTableShoe
    { _tsAttrs :: Delta 'TableShoe (Part 'Attrs)
    , _tsModes :: Delta 'TableShoe (Part 'Modes)
    , _tsRels :: Delta 'TableShoe (Part 'Rels)
    }
    deriving (Eq, Show, Generic)

data instance Delta 'TableShoe (Part 'Attrs)
    = DTableShoeSetCardStateMap (Map CardIx CardState) (Map CardIx CardState)
    | DTableShoeSetCardFate CardIx CardState CardState
    deriving (Eq, Show, Generic)

data instance Delta 'TableShoe (Part 'Modes)
    deriving (Eq, Show, Generic)

data instance Delta 'TableShoe (Part 'Rels)
    = DTableShoeSetTable (EntityRef 'Table) (EntityRef 'Table)
    deriving (Eq, Show, Generic)

instance ToJSON (Delta 'TableShoe 'Whole)
instance FromJSON (Delta 'TableShoe 'Whole)

instance ToJSON (Delta 'TableShoe (Part 'Attrs))
instance FromJSON (Delta 'TableShoe (Part 'Attrs))

instance ToJSON (Delta 'TableShoe (Part 'Modes))
instance FromJSON (Delta 'TableShoe (Part 'Modes))

instance ToJSON (Delta 'TableShoe (Part 'Rels))
instance FromJSON (Delta 'TableShoe (Part 'Rels))

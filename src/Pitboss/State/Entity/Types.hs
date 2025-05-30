{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Pitboss.State.Entity.Types (
    EntityState (..),
    IntentAttrs (..),
    IntentModes (..),
    IntentRels (..),
    EventAttrs (..),
    EventModes (..),
    EventRels (..),
    BoutAttrs (..),
    BoutModes (..),
    BoutRels (..),
    DealerAttrs (..),
    DealerModes (..),
    DealerRels (..),
    DealerHandAttrs (..),
    DealerHandModes (..),
    DealerHandRels (..),
    DealerRoundAttrs (..),
    DealerRoundModes (..),
    DealerRoundRels (..),
    OfferingAttrs (..),
    OfferingModes (..),
    OfferingRels (..),
    PlayerAttrs (..),
    PlayerModes (..),
    PlayerRels (..),
    PlayerHandAttrs (..),
    PlayerHandModes (..),
    PlayerHandRels (..),
    PlayerSpotAttrs (..),
    PlayerSpotModes (..),
    PlayerSpotRels (..),
    TableAttrs (..),
    TableModes (..),
    TableRels (..),
    TableShoeAttrs (..),
    TableShoeModes (..),
    TableShoeRels (..),
) where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Map.Strict
import GHC.Generics (Generic)
import Pitboss.Blackjack.Materia.Card (Card)
import Pitboss.Blackjack.Materia.Chips
import Pitboss.Blackjack.Materia.Hand (SomeHand)
import Pitboss.Blackjack.Offering qualified as O
import Pitboss.Blackjack.Play (Outcome)
import Pitboss.FSM.Bout (SomeBoutFSM)
import Pitboss.FSM.DealerHand
import Pitboss.FSM.DealerRound
import Pitboss.FSM.DealerTable
import Pitboss.FSM.PlayerHand
import Pitboss.FSM.PlayerSpot
import Pitboss.FSM.PlayerTable
import Pitboss.FSM.Table
import Pitboss.State.Types.Core
import Pitboss.State.Types.FiniteMap
import Pitboss.State.Types.FiniteMap.Occupancy

data family EntityState (k :: EntityKind)

-- EIntent
data IntentAttrs = IntentAttrs
    { _intentAttrsType :: IntentType
    , _intentAttrsDetails :: IntentDetails
    , _intentAttrsTimestamp :: Tick
    , _intentAttrsDescription :: String
    }
    deriving (Eq, Show, Generic)

data IntentModes = IntentModes
    deriving (Eq, Show, Generic)

data IntentRels = IntentRels
    { _intentRelsOriginatingEntity :: OriginatingEntity
    , _intentRelsTargetBout :: Maybe (EntityId 'Bout)
    }
    deriving (Eq, Show, Generic)

data instance EntityState 'Intent = EIntent
    { _intentAttrs :: IntentAttrs
    , _intentModes :: IntentModes
    , _intentRels :: IntentRels
    }
    deriving (Eq, Show, Generic)

-- EEvent
data EventAttrs = EventAttrs
    { _eventAttrsType :: EventType
    , _eventAttrsDetails :: EventDetails
    , _eventAttrsTimestamp :: Tick
    , _eventAttrsDescription :: String
    }
    deriving (Eq, Show, Generic)

data EventModes = EventModes
    deriving (Eq, Show, Generic)

data EventRels = EventRels
    { _eventRelsCausingIntent :: EntityId 'Intent
    }
    deriving (Eq, Show, Generic)

data instance EntityState 'Event = EEvent
    { _eventAttrs :: EventAttrs
    , _eventModes :: EventModes
    , _eventRels :: EventRels
    }
    deriving (Eq, Show, Generic)

-- EBout
data BoutAttrs = BoutAttrs
    { _boutAttrsOutcome :: Maybe Outcome
    }
    deriving (Eq, Show, Generic)

data BoutModes = BoutModes
    { _boutModesFSM :: SomeBoutFSM
    }
    deriving (Eq, Show, Generic)

data BoutRels = BoutRels
    { _boutRelsPlayerHand :: EntityId 'PlayerHand
    , _boutRelsDealerHand :: EntityId 'DealerHand
    , _boutRelsTableShoe :: EntityId 'TableShoe
    }
    deriving (Eq, Show, Generic)

data instance EntityState 'Bout = EBout
    { _boutAttrs :: BoutAttrs
    , _boutModes :: BoutModes
    , _boutRels :: BoutRels
    }
    deriving (Eq, Show, Generic)

-- EDealer
data DealerAttrs = DealerAttrs
    { _dAttrsName :: String
    }
    deriving (Eq, Show, Generic)

data DealerModes = DealerModes
    { _dModesDealerTable :: SomeDealerTableFSM
    , _dModesDealerRound :: DealerRoundFSM
    , _dModesDealerHand :: SomeDealerHandFSM
    }
    deriving (Eq, Show, Generic)

data DealerRels = DealerRels
    { _dRelsActiveTable :: Maybe (EntityId 'Table)
    , _dRelsActiveRound :: Maybe (EntityId 'DealerRound)
    , _dRelsActiveHand :: Maybe (EntityId 'DealerHand)
    }
    deriving (Eq, Show, Generic)

data instance EntityState 'Dealer = EDealer
    { _dAttrs :: DealerAttrs
    , _dModes :: DealerModes
    , _dRels :: DealerRels
    }
    deriving (Eq, Show, Generic)

-- EDealerHand
data DealerHandAttrs = DealerHandAttrs
    {_dhAttrsHand :: SomeHand}
    deriving (Eq, Show, Generic)

data DealerHandModes = DealerHandModes
    { _dhModesDealerHand :: SomeDealerHandFSM
    }
    deriving (Eq, Show, Generic)

data DealerHandRels = DealerHandRels
    { _dhRelsDealerRound :: EntityId 'DealerRound
    , _dhRelsDealer :: EntityId 'Dealer
    }
    deriving (Eq, Show, Generic)

data instance EntityState 'DealerHand = EDealerHand
    { _dhAttrs :: DealerHandAttrs
    , _dhModes :: DealerHandModes
    , _dhRels :: DealerHandRels
    }
    deriving (Eq, Show, Generic)

-- EDealerRound
data DealerRoundAttrs = DealerRoundAttrs
    { _drAttrsNumber :: Int
    , _drAttrsIsActive :: Bool
    }
    deriving (Eq, Show, Generic)

data DealerRoundModes = DealerRoundModes
    deriving (Eq, Show, Generic)

data DealerRoundRels = DealerRoundRels
    { _drRelsTableShoeUsed :: EntityId 'TableShoe
    }
    deriving (Eq, Show, Generic)

data instance EntityState 'DealerRound = EDealerRound
    { _drAttrs :: DealerRoundAttrs
    , _drModes :: DealerRoundModes
    , _drRels :: DealerRoundRels
    }
    deriving (Eq, Show, Generic)

-- EOffering
data OfferingAttrs = OfferingAttrs
    { _oAttrsOffering :: O.Offering
    }
    deriving (Eq, Show, Generic)

data OfferingModes = OfferingModes
    deriving (Eq, Show, Generic)

data OfferingRels = OfferingRels
    deriving (Eq, Show, Generic)

data instance EntityState 'Offering = EOffering
    { _oAttrs :: OfferingAttrs
    , _oModes :: OfferingModes
    , _oRels :: OfferingRels
    }
    deriving (Eq, Show, Generic)

-- EPlayer
data PlayerAttrs = PlayerAttrs
    { _pAttrsName :: String
    , _pAttrsBankroll :: Chips
    }
    deriving (Eq, Show, Generic)

data PlayerModes = PlayerModes
    { _pModesPlayerTable :: SomePlayerTableFSM
    , _pModesPlayerSpot :: SomePlayerSpotFSM
    , _pModesPlayerHand :: SomePlayerHandFSM
    }
    deriving (Eq, Show, Generic)

data PlayerRels = PlayerRels
    deriving (Eq, Show, Generic)

data instance EntityState 'Player = EPlayer
    { _pAttrs :: PlayerAttrs
    , _pModes :: PlayerModes
    , _pRels :: PlayerRels
    }
    deriving (Eq, Show, Generic)

-- EPlayerHand
data PlayerHandAttrs = PlayerHandAttrs
    { _phAttrsHand :: SomeHand
    , _phAttrsOriginalBet :: Chips
    , _phAttrsSplitDepth :: Int
    , _phAttrsHandIx :: Int
    }
    deriving (Eq, Show, Generic)

data PlayerHandModes = PlayerHandModes
    { _phFsm :: SomePlayerHandFSM
    }
    deriving (Eq, Show, Generic)

data PlayerHandRels = PlayerHandRels
    { _phRelsBelongsToPlayerSpot :: EntityId 'PlayerSpot
    , _phRelsBelongsToDealerRound :: EntityId 'DealerRound
    , _phRelsOwnedByPlayer :: EntityId 'Player
    }
    deriving (Eq, Show, Generic)

data instance EntityState 'PlayerHand = EPlayerHand
    { _phAttrs :: PlayerHandAttrs
    , _phModes :: PlayerHandModes
    , _phRels :: PlayerHandRels
    }
    deriving (Eq, Show, Generic)

-- EPlayerSpot
data PlayerSpotAttrs = PlayerSpotAttrs
    { _psAttrsSpotIndex :: PlayerSpotIx
    , _psAttrsWager :: Chips
    }
    deriving (Eq, Show, Generic)

data PlayerSpotModes = PlayerSpotModes
    { _psModesPlayerSpot :: SomePlayerSpotFSM
    }
    deriving (Eq, Show, Generic)

data PlayerSpotRels = PlayerSpotRels
    { _psEntityRelsPlayerId :: EntityId 'Player
    , _psEntityRelsRoundId :: EntityId 'DealerRound
    , _psRelsHandOccupancy :: FiniteMap PlayerSpotHandIx (Occupancy (EntityId 'PlayerHand))
    }
    deriving (Eq, Show, Generic)

data instance EntityState 'PlayerSpot = EPlayerSpot
    { _psAttrs :: PlayerSpotAttrs
    , _psModes :: PlayerSpotModes
    , _psRels :: PlayerSpotRels
    }
    deriving (Eq, Show, Generic)

-- ETable
data TableAttrs = TableAttrs
    { _tAttrsName :: String
    , _tAttrsCurrentRound :: Maybe (EntityId 'DealerRound)
    , _tAttrsOfferingUsed :: EntityId 'Offering
    , _tAttrsMinBet :: Chips
    }
    deriving (Eq, Show, Generic)

data TableModes = TableModes
    { _tModesFSM :: SomeTableFSM
    }
    deriving (Eq, Show, Generic)

data TableRels = TableRels
    { _tRelsManagedByDealer :: Maybe (EntityId 'Dealer)
    }
    deriving (Eq, Show, Generic)

data instance EntityState 'Table = ETable
    { _tAttrs :: TableAttrs
    , _tModes :: TableModes
    , _tRels :: TableRels
    }
    deriving (Eq, Show, Generic)

-- ETableShoe
data TableShoeAttrs = TableShoeAttrs
    { _tsAttrsCards :: [Card]
    , _tsAttrsCardStates :: Map CardIx CardState
    }
    deriving (Eq, Show, Generic)

data TableShoeModes = TableShoeModes
    deriving (Eq, Show, Generic)

data TableShoeRels = TableShoeRels
    { _tsRelsTable :: EntityId 'Table
    }
    deriving (Eq, Show, Generic)

data instance EntityState 'TableShoe = ETableShoe
    { _tsAttrs :: TableShoeAttrs
    , _tsModes :: TableShoeModes
    , _tsRels :: TableShoeRels
    }
    deriving (Eq, Show, Generic)

instance ToJSON IntentAttrs
instance FromJSON IntentAttrs
instance ToJSON IntentModes
instance FromJSON IntentModes
instance ToJSON IntentRels
instance FromJSON IntentRels
instance ToJSON (EntityState 'Intent)
instance FromJSON (EntityState 'Intent)

instance ToJSON EventAttrs
instance FromJSON EventAttrs
instance ToJSON EventModes
instance FromJSON EventModes
instance ToJSON EventRels
instance FromJSON EventRels
instance ToJSON (EntityState 'Event)
instance FromJSON (EntityState 'Event)

instance ToJSON BoutAttrs
instance FromJSON BoutAttrs
instance ToJSON BoutModes
instance FromJSON BoutModes
instance ToJSON BoutRels
instance FromJSON BoutRels
instance ToJSON (EntityState 'Bout)
instance FromJSON (EntityState 'Bout)

instance ToJSON DealerAttrs
instance FromJSON DealerAttrs
instance ToJSON DealerModes
instance FromJSON DealerModes
instance ToJSON DealerRels
instance FromJSON DealerRels
instance ToJSON (EntityState 'Dealer)
instance FromJSON (EntityState 'Dealer)

instance ToJSON DealerHandAttrs
instance FromJSON DealerHandAttrs
instance ToJSON DealerHandModes
instance FromJSON DealerHandModes
instance ToJSON DealerHandRels
instance FromJSON DealerHandRels
instance ToJSON (EntityState 'DealerHand)
instance FromJSON (EntityState 'DealerHand)

instance ToJSON DealerRoundAttrs
instance FromJSON DealerRoundAttrs
instance ToJSON DealerRoundModes
instance FromJSON DealerRoundModes
instance ToJSON DealerRoundRels
instance FromJSON DealerRoundRels
instance ToJSON (EntityState 'DealerRound)
instance FromJSON (EntityState 'DealerRound)

instance ToJSON OfferingAttrs
instance FromJSON OfferingAttrs
instance ToJSON OfferingModes
instance FromJSON OfferingModes
instance ToJSON OfferingRels
instance FromJSON OfferingRels
instance ToJSON (EntityState 'Offering)
instance FromJSON (EntityState 'Offering)

instance ToJSON PlayerAttrs
instance FromJSON PlayerAttrs
instance ToJSON PlayerModes
instance FromJSON PlayerModes
instance ToJSON PlayerRels
instance FromJSON PlayerRels
instance ToJSON (EntityState 'Player)
instance FromJSON (EntityState 'Player)

instance ToJSON PlayerHandAttrs
instance FromJSON PlayerHandAttrs
instance ToJSON PlayerHandModes
instance FromJSON PlayerHandModes
instance ToJSON PlayerHandRels
instance FromJSON PlayerHandRels
instance ToJSON (EntityState 'PlayerHand)
instance FromJSON (EntityState 'PlayerHand)

instance ToJSON PlayerSpotAttrs
instance FromJSON PlayerSpotAttrs
instance ToJSON PlayerSpotModes
instance FromJSON PlayerSpotModes
instance ToJSON PlayerSpotRels
instance FromJSON PlayerSpotRels
instance ToJSON (EntityState 'PlayerSpot)
instance FromJSON (EntityState 'PlayerSpot)

instance ToJSON TableAttrs
instance FromJSON TableAttrs
instance ToJSON TableModes
instance FromJSON TableModes
instance ToJSON TableRels
instance FromJSON TableRels
instance ToJSON (EntityState 'Table)
instance FromJSON (EntityState 'Table)

instance ToJSON TableShoeAttrs
instance FromJSON TableShoeAttrs
instance ToJSON TableShoeModes
instance FromJSON TableShoeModes
instance ToJSON TableShoeRels
instance FromJSON TableShoeRels
instance ToJSON (EntityState 'TableShoe)
instance FromJSON (EntityState 'TableShoe)

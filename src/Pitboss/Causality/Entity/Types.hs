{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Pitboss.Causality.Entity.Types where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Map.Strict
import GHC.Generics (Generic)
import Pitboss.Blackjack
import Pitboss.Causality.Types.Core
import Pitboss.Causality.Types.FiniteMap
import Pitboss.Causality.Types.FiniteMap.Occupancy
import Pitboss.FSM

data family EntityState (k :: EntityKind)

-- EBout
data BoutAttrs = BoutAttrs
    { _boutAttrsOutcome :: Maybe DetailedOutcome
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
    , _boutRelsTable :: EntityId 'Table
    , _boutRelsDealerRound :: EntityId 'DealerRound
    }
    deriving (Eq, Show, Generic)

data instance EntityState 'Bout = EBout
    { _boutAttrs :: BoutAttrs
    , _boutModes :: BoutModes
    , _boutRels :: BoutRels
    }
    deriving (Eq, Generic)

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
    deriving (Eq, Generic)

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
    deriving (Eq, Generic)

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
    deriving (Eq, Generic)

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
    deriving (Eq, Generic)

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
    , _phRelsBelongsToBout :: EntityId 'Bout
    }
    deriving (Eq, Show, Generic)

data instance EntityState 'PlayerHand = EPlayerHand
    { _phAttrs :: PlayerHandAttrs
    , _phModes :: PlayerHandModes
    , _phRels :: PlayerHandRels
    }
    deriving (Eq, Generic)

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
    deriving (Eq, Generic)

-- ETable
data TableAttrs = TableAttrs
    { _tAttrsName :: String
    , _tAttrsCurrentRound :: Maybe (EntityId 'DealerRound)
    , _tAttrsOffering :: Offering
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
    deriving (Eq, Generic)

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
    deriving (Eq, Generic)

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

instance Show (EntityState 'Bout) where
    show (EBout attrs modes rels) =
        "EBout " ++ show attrs ++ " " ++ show modes ++ " " ++ show rels

instance Show (EntityState 'Dealer) where
    show (EDealer attrs modes rels) =
        "EDealer " ++ show attrs ++ " " ++ show modes ++ " " ++ show rels

instance Show (EntityState 'DealerHand) where
    show (EDealerHand attrs modes rels) =
        "EDealerHand " ++ show attrs ++ " " ++ show modes ++ " " ++ show rels

instance Show (EntityState 'DealerRound) where
    show (EDealerRound attrs modes rels) =
        "EDealerRound " ++ show attrs ++ " " ++ show modes ++ " " ++ show rels

instance Show (EntityState 'Player) where
    show (EPlayer attrs modes rels) =
        "EPlayer " ++ show attrs ++ " " ++ show modes ++ " " ++ show rels

instance Show (EntityState 'PlayerHand) where
    show (EPlayerHand attrs modes rels) =
        "EPlayerHand " ++ show attrs ++ " " ++ show modes ++ " " ++ show rels

instance Show (EntityState 'PlayerSpot) where
    show (EPlayerSpot attrs modes rels) =
        "EPlayerSpot " ++ show attrs ++ " " ++ show modes ++ " " ++ show rels

instance Show (EntityState 'Table) where
    show (ETable attrs modes rels) =
        "ETable " ++ show attrs ++ " " ++ show modes ++ " " ++ show rels

instance Show (EntityState 'TableShoe) where
    show (ETableShoe attrs modes rels) =
        "ETableShoe " ++ show attrs ++ " " ++ show modes ++ " " ++ show rels

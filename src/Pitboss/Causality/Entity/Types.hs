{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Pitboss.Causality.Entity.Types where

import Data.Aeson.Types
import Data.Map.Strict (Map)
import GHC.Generics (Generic, Rep)
import Pitboss.Blackjack hiding (Hand)
import Pitboss.Causality.Types.Core
import Pitboss.Causality.Types.FiniteMap
import Pitboss.FSM

{-
Entity Relationship Graph (Unidirectional):

Table ──[seats:FiniteMap]──> TableSeat (embedded)
  │
  ├──[activeDealer:Occupancy]─> Dealer ──[activeHand:Occupancy]─> Hand
  │                               │
  │                               ├──[activeRound:Occupancy]────> Round
  │                               │
  │                               └──[activeTable:Occupancy]────> Table
  │
  └──[activeRound:Occupancy]──> Round ──[contestants:FiniteMap]─> Contestant
                                  │
                                  ├──[dealerHand:Occupancy]─────> Hand
                                  │
                                  ├──[shoe]──> Shoe ──[table]───> Table
                                  │
                                  └──[table]────────────────────> Table

Player ──[activeTable:Occupancy]──> Table
  │
  └──────[activeRound:Occupancy]──> Round

Contestant ──[bouts:FiniteMap]──> Bout ──[playerHand]───> Hand
         │                          │
         ├──[player]──> Player      ├────[dealerHand]──> Hand
         │                          │
         ├──[round]───> Round       ├────[round]───────> Round
         │                          │
         ├──[shoe]────> Shoe        ├────[shoe]────────> Shoe
         │                          │
         └──[table]───> Table       └────[table]───────> Table

Hand ──[owner:HandOwner]──> ContestantOwner(Contestant)
  │                  │
  ├──[bout]──> Bout  └────> DealerOwner(Dealer)
  │
  └──[shoe]──> Shoe

Key Design Principles:
1. All relationships are unidirectional (no back-references)
2. FiniteMap for bounded collections (Table→Seats, Round→Contestants, Contestant→Bouts)
3. Occupancy for temporal presence/absence semantics
4. Bout coordinates 1v1 contest between player hand and dealer hand
5. Hand ownership through sealed HandOwner sum type
6. Contestant manages append-only bout collection via FiniteMap
7. Hand entities own FSM state trails for lifecycle coherence
8. Shoe shared across entities for card state consistency
-}

newtype AsJSON a = AsJSON a

instance (Generic a, GToJSON Zero (Rep a)) => ToJSON (AsJSON a) where
    toJSON (AsJSON a) = genericToJSON defaultOptions a

instance (Generic a, GFromJSON Zero (Rep a)) => FromJSON (AsJSON a) where
    parseJSON = fmap AsJSON . genericParseJSON defaultOptions

data family EntityState (k :: EntityKind)

data SomeHandFSM
    = ContestantHandFSM SomeContestantHandFSM
    | DealerHandFSM SomeDealerHandFSM
    deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via AsJSON SomeHandFSM

data BoutAttrs = BoutAttrs
    { _bAttrsOutcome :: Occupancy DetailedOutcome
    }
    deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via AsJSON BoutAttrs

data BoutModes = BoutModes
    { _bModesFSM :: SomeBoutFSM
    }
    deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via AsJSON BoutModes

data BoutRels = BoutRels
    { _bRelsPlayerHand :: HandId
    , _bRelsDealerHand :: HandId
    , _bRelsRound :: RoundId
    , _bRelsShoe :: ShoeId
    , _bRelsTable :: TableId
    }
    deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via AsJSON BoutRels

data instance EntityState 'Bout = EBout
    { _bAttrs :: BoutAttrs
    , _bModes :: BoutModes
    , _bRels :: BoutRels
    }
    deriving stock (Eq, Generic)
    deriving (ToJSON, FromJSON) via AsJSON (EntityState 'Bout)

instance Show (EntityState 'Bout) where
    show (EBout attrs modes rels) =
        "EBout " ++ show attrs ++ " " ++ show modes ++ " " ++ show rels

data ContestantAttrs = ContestantAttrs
    { _cAttrsActiveHandIx :: HandIx
    }
    deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via AsJSON ContestantAttrs

data ContestantModes = ContestantModes
    { _cModesBoutFSM :: Occupancy SomeContestantBoutFSM
    , _cModesRoundFSM :: SomeContestantRoundFSM
    }
    deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via AsJSON ContestantModes

data ContestantRels = ContestantRels
    { _cRelsBouts :: FiniteMap HandIx (Occupancy BoutId)
    , _cRelsPlayer :: PlayerId
    , _cRelsRound :: RoundId
    , _cRelsShoe :: ShoeId
    }
    deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via AsJSON ContestantRels

data instance EntityState 'Contestant = EContestant
    { _cAttrs :: ContestantAttrs
    , _cModes :: ContestantModes
    , _cRels :: ContestantRels
    }
    deriving stock (Eq, Generic)
    deriving (ToJSON, FromJSON) via AsJSON (EntityState 'Contestant)

instance Show (EntityState 'Contestant) where
    show (EContestant attrs modes rels) =
        "EContestant " ++ show attrs ++ " " ++ show modes ++ " " ++ show rels

data DealerAttrs = DealerAttrs
    { _dAttrsName :: String
    }
    deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via AsJSON DealerAttrs

data DealerModes = DealerModes
    { _dModesDealerTable :: SomeDealerTableFSM
    , _dModesRound :: RoundFSM
    }
    deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via AsJSON DealerModes

data DealerRels = DealerRels
    { _dRelsActiveHand :: Occupancy HandId
    , _dRelsActiveRound :: Occupancy RoundId
    , _dRelsActiveTable :: Occupancy TableId
    }
    deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via AsJSON DealerRels

data instance EntityState 'Dealer = EDealer
    { _dAttrs :: DealerAttrs
    , _dModes :: DealerModes
    , _dRels :: DealerRels
    }
    deriving stock (Eq, Generic)
    deriving (ToJSON, FromJSON) via AsJSON (EntityState 'Dealer)

instance Show (EntityState 'Dealer) where
    show (EDealer attrs modes rels) =
        "EDealer " ++ show attrs ++ " " ++ show modes ++ " " ++ show rels

data HandOwner = ContestantOwner ContestantId | DealerOwner DealerId
    deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via AsJSON HandOwner

data HandAttrs = HandAttrs
    { _hAttrsHand :: SomeHand
    }
    deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via AsJSON HandAttrs

data HandModes = HandModes
    { _hModesHandFSM :: SomeHandFSM
    }
    deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via AsJSON HandModes

data HandRels = HandRels
    { _hRelsOwner :: HandOwner
    , _hRelsBout :: BoutId
    , _hRelsShoe :: ShoeId
    }
    deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via AsJSON HandRels

data instance EntityState 'Hand = EHand
    { _hAttrs :: HandAttrs
    , _hModes :: HandModes
    , _hRels :: HandRels
    }
    deriving stock (Eq, Generic)
    deriving (ToJSON, FromJSON) via AsJSON (EntityState 'Hand)

instance Show (EntityState 'Hand) where
    show (EHand attrs modes rels) =
        "EHand " ++ show attrs ++ " " ++ show modes ++ " " ++ show rels

data PlayerAttrs = PlayerAttrs
    { _pAttrsName :: String
    , _pAttrsBankroll :: Chips
    }
    deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via AsJSON PlayerAttrs

data PlayerModes = PlayerModes
    { _pModesPlayerFSM :: SomePlayerFSM
    }
    deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via AsJSON PlayerModes

data PlayerRels = PlayerRels
    { _pRelsActiveRound :: Occupancy RoundId
    , _pRelsActiveTable :: Occupancy TableId
    }
    deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via AsJSON PlayerRels

data instance EntityState 'Player = EPlayer
    { _pAttrs :: PlayerAttrs
    , _pModes :: PlayerModes
    , _pRels :: PlayerRels
    }
    deriving stock (Eq, Generic)
    deriving (ToJSON, FromJSON) via AsJSON (EntityState 'Player)

instance Show (EntityState 'Player) where
    show (EPlayer attrs modes rels) =
        "EPlayer " ++ show attrs ++ " " ++ show modes ++ " " ++ show rels

data RoundAttrs = RoundAttrs
    { _rAttrsActiveSpotIx :: Occupancy TableSpotIx
    , _rAttrsNumber :: Int
    }
    deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via AsJSON RoundAttrs

data RoundModes = RoundModes
    deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via AsJSON RoundModes

data RoundRels = RoundRels
    { _rRelsDealerHand :: Occupancy HandId
    , _rRelsContestants :: FiniteMap TableSpotIx (Occupancy ContestantId)
    , _rRelsShoe :: ShoeId
    , _rRelsTable :: TableId
    }
    deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via AsJSON RoundRels

data instance EntityState 'Round = ERound
    { _rAttrs :: RoundAttrs
    , _rModes :: RoundModes
    , _rRels :: RoundRels
    }
    deriving stock (Eq, Generic)
    deriving (ToJSON, FromJSON) via AsJSON (EntityState 'Round)

instance Show (EntityState 'Round) where
    show (ERound attrs modes rels) =
        "ERound " ++ show attrs ++ " " ++ show modes ++ " " ++ show rels

data ShoeAttrs = ShoeAttrs
    { _sAttrsCards :: [Card]
    , _sAttrsCardStates :: Map CardIx CardState
    }
    deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via AsJSON ShoeAttrs

data ShoeModes = ShoeModes
    deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via AsJSON ShoeModes

data ShoeRels = ShoeRels
    { _sRelsTable :: TableId
    }
    deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via AsJSON ShoeRels

data instance EntityState 'Shoe = EShoe
    { _sAttrs :: ShoeAttrs
    , _sModes :: ShoeModes
    , _sRels :: ShoeRels
    }
    deriving stock (Eq, Generic)
    deriving (ToJSON, FromJSON) via AsJSON (EntityState 'Shoe)

instance Show (EntityState 'Shoe) where
    show (EShoe attrs modes rels) =
        "EShoe " ++ show attrs ++ " " ++ show modes ++ " " ++ show rels

data TableSeat = TableSeat
    { _seatOccupant :: PlayerId
    , _seatReserved :: Bool
    }
    deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via AsJSON TableSeat

data TableAttrs = TableAttrs
    { _tAttrsName :: String
    , _tAttrsOffering :: Offering
    , _tAttrsSeats :: FiniteMap TableSpotIx (Occupancy TableSeat)
    }
    deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via AsJSON TableAttrs

data TableModes = TableModes
    { _tModesFSM :: SomeTableFSM
    }
    deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via AsJSON TableModes

data TableRels = TableRels
    { _tRelsActiveDealer :: Occupancy DealerId
    , _tRelsActiveRound :: Occupancy RoundId
    }
    deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via AsJSON TableRels

data instance EntityState 'Table = ETable
    { _tAttrs :: TableAttrs
    , _tModes :: TableModes
    , _tRels :: TableRels
    }
    deriving stock (Eq, Generic)
    deriving (ToJSON, FromJSON) via AsJSON (EntityState 'Table)

instance Show (EntityState 'Table) where
    show (ETable attrs modes rels) =
        "ETable " ++ show attrs ++ " " ++ show modes ++ " " ++ show rels

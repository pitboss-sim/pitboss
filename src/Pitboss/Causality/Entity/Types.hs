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
import Pitboss.Blackjack
import Pitboss.Causality.Types.Core
import Pitboss.Causality.Types.FiniteMap
import Pitboss.FSM.Types

{-

-----------------------------------------------------------------------------------------------------------------------

   Entity Relationship Graph (Unidirectional)

   Table ──[seats:FiniteMap]──> TableSeat (embedded)
     │                           └──[occupant]──> Player
     │
     ├──[activeDealer:Occupancy]─> Dealer ──[activeRound:Occupancy]──> Round
     │                               │
     │                               └──[activeTable:Occupancy]──────> Table
     │
     └──[activeRound:Occupancy]──> Round ──[bouts:FiniteMap]───────────────> Bout
                                     │                                        │
                                     ├──[shoe]─> Shoe ──[table]─────> Table   │
                                     │                                        │
                                     └──[table]─────────────────────> Table   │
                                                                              │
                                                                              ├──[player]───> Player
                                                                              │
                                                                              ├──[dealer]───> Dealer
                                                                              │
    Player ──[activeTable:Occupancy]──> Table                                 ├──[round]────> Round
      │                                                                       │
      └──[activeRound:Occupancy]──> Round                                     ├──[table]────> Table
                                                                              │
    Dealer ──[activeTable:Occupancy]──> Table                                 ├──[playerHand]> SomeHand (embedded)
      │                                                                       │
      └──[activeRound:Occupancy]──> Round                                     ├──[dealerHand]> SomeHand (embedded)
                                                                              │
    Shoe ──[table]──> Table                                                   └──[playerBouts]> Bout (splits)

-----------------------------------------------------------------------------------------------------------------------

Key Design Principles:
1. All relationships are unidirectional (no back-references)
2. FiniteMap for bounded collections with enum keys:
   - Table→TableSeats (by TableSpotIx)
   - Round→Bouts (by TableSpotIx)
   - Bout→PlayerBouts (by HandIx for splits)
3. Occupancy for temporal presence/absence semantics
4. Bout is central coordinator containing:
   - References to Player, Dealer, Round, Table
   - Embedded SomeHand data for both player and dealer
   - Split hand management via playerBouts FiniteMap
5. No Hand entity - hands are embedded SomeHand data in Bout
6. TableSeat embedded in Table with direct Player reference
7. FSM state managed in entity Modes sections
8. Shoe shared reference point for card state consistency
9. Strict hierarchy: Table > Round > Bout > SomeHand (embedded)

Entity FSM State:
- Table: SomeTableFSM
- Round: RoundFSM (ENHC/Peek variants)
- Player: SomePlayerFSM
- Dealer: SomeDealerTableFSM
- Bout: SomeBoutFSM + SomePlayerHandFSM + SomeDealerHandFSM
- Shoe: [No FSM, but stateful]

-}

newtype AsJSON a = AsJSON a

instance (Generic a, GToJSON Zero (Rep a)) => ToJSON (AsJSON a) where
    toJSON (AsJSON a) = genericToJSON defaultOptions a

instance (Generic a, GFromJSON Zero (Rep a)) => FromJSON (AsJSON a) where
    parseJSON = fmap AsJSON . genericParseJSON defaultOptions

data family EntityState (k :: EntityKind)

data BoutAttrs = BoutAttrs
    { _bAttrsPlayerHand :: SomeHand
    , _bAttrsDealerHand :: SomeHand
    , _bAttrsActiveHandIx :: HandIx
    , _bAttrsOutcome :: Occupancy DetailedOutcome
    }
    deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via AsJSON BoutAttrs

data BoutModes = BoutModes
    { _bModesBoutFSM :: SomeBoutFSM
    , _bModesPlayerHandFSM :: SomePlayerHandFSM
    , _bModesDealerHandFSM :: SomeDealerHandFSM
    }
    deriving stock (Eq, Show, Generic)
    deriving (ToJSON, FromJSON) via AsJSON BoutModes

data BoutRels = BoutRels
    { _bRelsPlayer :: PlayerId
    , _bRelsDealer :: DealerId
    , _bRelsRound :: RoundId
    , _bRelsTable :: TableId
    , _bRelsPlayerBouts :: FiniteMap HandIx (Occupancy BoutId)
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

data DealerAttrs = DealerAttrs
    { _dAttrsName :: String
    , _dAttrsStagedCard :: Occupancy Card
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
    { _dRelsActiveRound :: Occupancy RoundId
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
    { _rRelsBouts :: FiniteMap TableSpotIx (Occupancy BoutId)
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

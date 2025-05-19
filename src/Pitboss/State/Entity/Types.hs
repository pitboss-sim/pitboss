{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Pitboss.State.Entity.Types (
    EntityKind (..),
    EntityRef (..),
    EntityState (..),
    EntityStateSelector (..),
    EntityStatePart (..),
    Id,
    CardIx,
    CardState,
    PlayerSpotHandIx,
    mkEDealer,
    mkEDealerAttrs,
    mkEDealerModes,
    mkEDealerRels,
    mkEDealerHand,
    mkEDealerHandAttrs,
    mkEDealerHandModes,
    mkEDealerHandRels,
    mkEDealerRound,
    mkEDealerRoundAttrs,
    mkEDealerRoundModes,
    mkEDealerRoundRels,
    mkEOffering,
    mkEOfferingAttrs,
    mkEOfferingModes,
    mkEOfferingRels,
    mkEPlayer,
    mkEPlayerAttrs,
    mkEPlayerModes,
    mkEPlayerHand,
    mkEPlayerHandAttrs,
    mkEPlayerHandModes,
    mkEPlayerHandRels,
    mkEPlayerRels,
    mkEPlayerSpot,
    mkEPlayerSpotAttrs,
    mkEPlayerSpotModes,
    mkEPlayerSpotRels,
    mkETable,
    mkETableAttrs,
    mkETableModes,
    mkETableRels,
    mkETableShoe,
    mkETableShoeAttrs,
    mkETableShoeModes,
    mkETableShoeRels,
) where

import Control.Monad (when)
import Data.Aeson (
    FromJSON (..),
    FromJSONKey,
    KeyValue ((.=)),
    ToJSON (..),
    ToJSONKey,
    object,
    withObject,
    (.:),
 )
import Data.Aeson.Types (Parser)
import Data.Map.Strict
import Data.Proxy (Proxy (..))
import Data.Text qualified as T
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
import Pitboss.State.Types.FiniteMap
import Pitboss.State.Types.FiniteMap.BoundedEnum
import Pitboss.State.Types.FiniteMap.Occupancy
import Pitboss.State.Types.Tick
import Pitboss.State.Types.Uid

data family EntityState (k :: EntityKind) (s :: EntityStateSelector)

data EntityKind
    = Dealer
    | DealerHand
    | DealerRound
    | Offering
    | Player
    | PlayerHand
    | PlayerSpot
    | Table
    | TableShoe
    deriving (Eq, Show, Generic)

data EntityStatePart = Meta | Attrs | Modes | Rels
    deriving (Eq, Show, Generic)

data EntityStateSelector
    = Whole
    | Part EntityStatePart
    deriving (Eq, Show, Generic)

data family Metadata (k :: EntityKind)

data instance Metadata k = Metadata
    { _id :: EntityRef k
    , _bornAt :: Maybe Tick
    }
    deriving (Eq, Show, Generic)

instance FromJSON (Metadata k)
instance ToJSON (Metadata k)

type family Id (k :: EntityKind)
type instance Id k = Uid

class KnownEntityKind (k :: EntityKind) where
    entityTag :: Proxy k -> String

instance KnownEntityKind 'Dealer where
    entityTag _ = "Dealer"
instance KnownEntityKind 'DealerHand where
    entityTag _ = "DealerHand"
instance KnownEntityKind 'DealerRound where
    entityTag _ = "DealerRound"
instance KnownEntityKind 'Offering where
    entityTag _ = "Offering"
instance KnownEntityKind 'Player where
    entityTag _ = "Player"
instance KnownEntityKind 'PlayerHand where
    entityTag _ = "PlayerHand"
instance KnownEntityKind 'PlayerSpot where
    entityTag _ = "PlayerSpot"
instance KnownEntityKind 'Table where
    entityTag _ = "Table"
instance KnownEntityKind 'TableShoe where
    entityTag _ = "TableShoe"

newtype TaggedId (k :: EntityKind) = TaggedId Uid

instance (KnownEntityKind k) => ToJSON (TaggedId k) where
    toJSON (TaggedId uid) =
        object
            [ "tag" .= entityTag (Proxy @k)
            , "uid" .= uid
            ]

instance (KnownEntityKind k) => FromJSON (TaggedId k) where
    parseJSON = withObject "TaggedId" $ \o -> do
        tag <- o .: "tag" :: Parser T.Text
        uid <- o .: "uid"
        let expected = T.pack $ entityTag (Proxy @k)
        when (tag /= expected) $
            fail $
                "Expected tag " ++ T.unpack expected ++ ", but got " ++ T.unpack tag
        pure $ TaggedId uid

newtype EntityRef (k :: EntityKind)
    = Clocked (Id k)
    deriving (Eq, Ord, Show, Generic)

instance (ToJSON (EntityRef k)) => ToJSON (EntityRef k) where
    toJSON (Clocked x) = object ["variant" .= ("Clocked" :: String), "value" .= x]

instance (FromJSON (EntityRef k)) => FromJSON (EntityRef k) where
    parseJSON = withObject "EntityRef" $ \o -> do
        variant <- o .: "variant"
        case variant :: String of
            "Clocked" -> Clocked <$> o .: "value"
            _ -> fail ("Unknown EntityRef variant: " ++ variant)

-- EDealer

mkEDealer ::
    EntityState 'Dealer (Part 'Meta) ->
    EntityState 'Dealer (Part 'Attrs) ->
    EntityState 'Dealer (Part 'Modes) ->
    EntityState 'Dealer (Part 'Rels) ->
    EntityState 'Dealer 'Whole
mkEDealer = EDealer

mkEDealerAttrs :: String -> EntityState 'Dealer (Part 'Attrs)
mkEDealerAttrs = EDealerAttrs

mkEDealerModes :: SomeDealerTableFSM -> DealerRoundFSM -> SomeDealerHandFSM -> EntityState 'Dealer (Part 'Modes)
mkEDealerModes = EDealerModes

mkEDealerRels ::
    Maybe (EntityRef 'Table) ->
    Maybe (EntityRef 'DealerRound) ->
    Maybe (EntityRef 'DealerHand) ->
    EntityState 'Dealer (Part 'Rels)
mkEDealerRels = EDealerRels

data instance EntityState 'Dealer 'Whole = EDealer
    { _dMeta :: EntityState 'Dealer (Part 'Meta)
    , _dAttrs :: EntityState 'Dealer (Part 'Attrs)
    , _dModes :: EntityState 'Dealer (Part 'Modes)
    , _dRels :: EntityState 'Dealer (Part 'Rels)
    }
    deriving (Eq, Show, Generic)

newtype instance EntityState 'Dealer (Part 'Meta)
    = EDealerMeta (Metadata 'Dealer)
    deriving (Eq, Show, Generic)

data instance EntityState 'Dealer (Part 'Attrs) = EDealerAttrs
    { _dAttrsName :: String
    }
    deriving (Eq, Show, Generic)

data instance EntityState 'Dealer (Part 'Modes) = EDealerModes
    { _dModesDealerTable :: SomeDealerTableFSM
    , _dModesDealerRound :: DealerRoundFSM
    , _dModesDealerHand :: SomeDealerHandFSM
    }
    deriving (Eq, Show, Generic)

data instance EntityState 'Dealer (Part 'Rels) = EDealerRels
    { _dRelsActiveTable :: Maybe (EntityRef 'Table)
    , _dRelsActiveRound :: Maybe (EntityRef 'DealerRound)
    , _dRelsActiveHand :: Maybe (EntityRef 'DealerHand)
    }
    deriving (Eq, Show, Generic)

instance ToJSON (EntityState 'Dealer 'Whole)
instance FromJSON (EntityState 'Dealer 'Whole)

instance ToJSON (EntityState 'Dealer (Part 'Meta))
instance FromJSON (EntityState 'Dealer (Part 'Meta))

instance ToJSON (EntityState 'Dealer (Part 'Attrs))
instance FromJSON (EntityState 'Dealer (Part 'Attrs))

instance ToJSON (EntityState 'Dealer (Part 'Modes))
instance FromJSON (EntityState 'Dealer (Part 'Modes))

instance ToJSON (EntityState 'Dealer (Part 'Rels))
instance FromJSON (EntityState 'Dealer (Part 'Rels))

-- EDealerHand

mkEDealerHand ::
    EntityState DealerHand (Part Meta) ->
    EntityState DealerHand (Part Attrs) ->
    EntityState DealerHand (Part Modes) ->
    EntityState DealerHand (Part Rels) ->
    EntityState DealerHand Whole
mkEDealerHand = EDealerHand

mkEDealerHandAttrs :: [Card] -> EntityState 'DealerHand (Part 'Attrs)
mkEDealerHandAttrs = EDealerHandAttrs

mkEDealerHandModes :: SomeDealerHandFSM -> EntityState 'DealerHand (Part 'Modes)
mkEDealerHandModes = EDealerHandModes

mkEDealerHandRels :: EntityRef 'DealerRound -> EntityRef 'Dealer -> EntityState 'DealerHand (Part 'Rels)
mkEDealerHandRels = EDealerHandRels

data instance EntityState 'DealerHand 'Whole = EDealerHand
    { _dhMeta :: EntityState 'DealerHand (Part 'Meta)
    , _dhAttrs :: EntityState 'DealerHand (Part 'Attrs)
    , _dhModes :: EntityState 'DealerHand (Part 'Modes)
    , _dhRels :: EntityState 'DealerHand (Part 'Rels)
    }
    deriving (Eq, Show, Generic)

newtype instance EntityState 'DealerHand (Part 'Meta)
    = EDealerHandMeta (Metadata 'DealerHand)
    deriving (Eq, Show, Generic)

data instance EntityState 'DealerHand (Part 'Attrs) = EDealerHandAttrs
    { _dhAttrsHandCards :: [Card]
    }
    deriving (Eq, Show, Generic)

data instance EntityState 'DealerHand (Part 'Modes) = EDealerHandModes
    { _dhModesDealerHand :: SomeDealerHandFSM
    }
    deriving (Eq, Show, Generic)

data instance EntityState 'DealerHand (Part 'Rels) = EDealerHandRels
    { _dhRelsDealerRound :: EntityRef 'DealerRound
    , _dhRelsDealer :: EntityRef 'Dealer
    }
    deriving (Eq, Show, Generic)

instance ToJSON (EntityState 'DealerHand 'Whole)
instance FromJSON (EntityState 'DealerHand 'Whole)

instance ToJSON (EntityState 'DealerHand (Part 'Meta))
instance FromJSON (EntityState 'DealerHand (Part 'Meta))

instance ToJSON (EntityState 'DealerHand (Part 'Attrs))
instance FromJSON (EntityState 'DealerHand (Part 'Attrs))

instance ToJSON (EntityState 'DealerHand (Part 'Modes))
instance FromJSON (EntityState 'DealerHand (Part 'Modes))

instance ToJSON (EntityState 'DealerHand (Part 'Rels))
instance FromJSON (EntityState 'DealerHand (Part 'Rels))

-- EDealerRound

mkEDealerRound ::
    EntityState DealerRound (Part Meta) ->
    EntityState DealerRound (Part Attrs) ->
    EntityState DealerRound (Part Modes) ->
    EntityState DealerRound (Part Rels) ->
    EntityState DealerRound Whole
mkEDealerRound = EDealerRound

mkEDealerRoundAttrs :: Int -> Bool -> EntityState 'DealerRound (Part 'Attrs)
mkEDealerRoundAttrs = EDealerRoundAttrs

mkEDealerRoundModes :: EntityState 'DealerRound (Part 'Modes)
mkEDealerRoundModes = EDealerRoundModes

mkEDealerRoundRels :: EntityRef 'TableShoe -> EntityState 'DealerRound (Part 'Rels)
mkEDealerRoundRels = EDealerRoundRels

data instance EntityState 'DealerRound 'Whole = EDealerRound
    { _drMeta :: EntityState 'DealerRound (Part 'Meta)
    , _drAttrs :: EntityState 'DealerRound (Part 'Attrs)
    , _drModes :: EntityState 'DealerRound (Part 'Modes)
    , _drRels :: EntityState 'DealerRound (Part 'Rels)
    }
    deriving (Eq, Show, Generic)

newtype instance EntityState 'DealerRound (Part 'Meta)
    = EDealerRoundMeta (Metadata 'DealerRound)
    deriving (Eq, Show, Generic)

data instance EntityState 'DealerRound (Part 'Attrs) = EDealerRoundAttrs
    { _drAttrsNumber :: Int
    , _drAttrsIsActive :: Bool
    }
    deriving (Eq, Show, Generic)

data instance EntityState 'DealerRound (Part 'Modes) = EDealerRoundModes
    deriving (Eq, Show, Generic)

data instance EntityState 'DealerRound (Part 'Rels) = EDealerRoundRels
    { _drRelsTableShoeUsed :: EntityRef 'TableShoe
    }
    deriving (Eq, Show, Generic)

instance ToJSON (EntityState 'DealerRound 'Whole)
instance FromJSON (EntityState 'DealerRound 'Whole)

instance ToJSON (EntityState 'DealerRound (Part 'Meta))
instance FromJSON (EntityState 'DealerRound (Part 'Meta))

instance ToJSON (EntityState 'DealerRound (Part 'Attrs))
instance FromJSON (EntityState 'DealerRound (Part 'Attrs))

instance ToJSON (EntityState 'DealerRound (Part 'Modes))
instance FromJSON (EntityState 'DealerRound (Part 'Modes))

instance ToJSON (EntityState 'DealerRound (Part 'Rels))
instance FromJSON (EntityState 'DealerRound (Part 'Rels))

-- EOffering

mkEOffering ::
    EntityState Offering (Part Meta) ->
    EntityState Offering (Part Attrs) ->
    EntityState Offering (Part Modes) ->
    EntityState Offering (Part Rels) ->
    EntityState Offering Whole
mkEOffering = EOffering

mkEOfferingAttrs :: O.Offering -> EntityState 'Offering (Part 'Attrs)
mkEOfferingAttrs = EOfferingAttrs

mkEOfferingModes :: EntityState 'Offering (Part 'Modes)
mkEOfferingModes = EOfferingModes

mkEOfferingRels :: EntityState 'Offering (Part 'Rels)
mkEOfferingRels = EOfferingRels

data instance EntityState 'Offering 'Whole = EOffering
    { _oMeta :: EntityState 'Offering (Part 'Meta)
    , _oAttrs :: EntityState 'Offering (Part 'Attrs)
    , _oModes :: EntityState 'Offering (Part 'Modes)
    , _oRels :: EntityState 'Offering (Part 'Rels)
    }
    deriving (Eq, Show, Generic)

newtype instance EntityState 'Offering (Part 'Meta)
    = EOfferingMeta (Metadata 'Offering)
    deriving (Eq, Show, Generic)

data instance EntityState 'Offering (Part 'Attrs) = EOfferingAttrs
    { _oAttrsOffering :: O.Offering
    }
    deriving (Eq, Show, Generic)

data instance EntityState 'Offering (Part 'Modes) = EOfferingModes
    deriving (Eq, Show, Generic)

data instance EntityState 'Offering (Part 'Rels) = EOfferingRels
    deriving (Eq, Show, Generic)

instance ToJSON (EntityState 'Offering 'Whole)
instance FromJSON (EntityState 'Offering 'Whole)

instance ToJSON (EntityState 'Offering (Part 'Meta))
instance FromJSON (EntityState 'Offering (Part 'Meta))

instance ToJSON (EntityState 'Offering (Part 'Attrs))
instance FromJSON (EntityState 'Offering (Part 'Attrs))

instance ToJSON (EntityState 'Offering (Part 'Modes))
instance FromJSON (EntityState 'Offering (Part 'Modes))

instance ToJSON (EntityState 'Offering (Part 'Rels))
instance FromJSON (EntityState 'Offering (Part 'Rels))

-- EPlayer

mkEPlayer ::
    EntityState 'Player (Part 'Meta) ->
    EntityState 'Player (Part 'Attrs) ->
    EntityState 'Player (Part 'Modes) ->
    EntityState 'Player (Part 'Rels) ->
    EntityState 'Player 'Whole
mkEPlayer = EPlayer

mkEPlayerAttrs :: String -> Chips -> EntityState 'Player (Part 'Attrs)
mkEPlayerAttrs = EPlayerAttrs

mkEPlayerModes ::
    SomePlayerTableFSM ->
    SomePlayerSpotFSM ->
    SomePlayerHandFSM ->
    EntityState Player (Part Modes)
mkEPlayerModes = EPlayerModes

mkEPlayerRels :: EntityState Player (Part Rels)
mkEPlayerRels = EPlayerRels

data instance EntityState 'Player 'Whole = EPlayer
    { _pMeta :: EntityState 'Player (Part 'Meta)
    , _pAttrs :: EntityState 'Player (Part 'Attrs)
    , _pModes :: EntityState 'Player (Part 'Modes)
    , _pRels :: EntityState 'Player (Part 'Rels)
    }
    deriving (Eq, Show, Generic)

newtype instance EntityState 'Player (Part 'Meta)
    = EPlayerMeta (Metadata 'Player)
    deriving (Eq, Show, Generic)

data instance EntityState 'Player (Part 'Attrs) = EPlayerAttrs
    { _pAttrsName :: String
    , _pAttrsBankroll :: Chips
    }
    deriving (Eq, Show, Generic)

data instance EntityState 'Player (Part 'Modes) = EPlayerModes
    { _pModesPlayerTable :: SomePlayerTableFSM
    , _pModesPlayerSpot :: SomePlayerSpotFSM
    , _pModesPlayerHand :: SomePlayerHandFSM
    }
    deriving (Eq, Show, Generic)

data instance EntityState 'Player (Part 'Rels) = EPlayerRels
    deriving (Eq, Show, Generic)

instance ToJSON (EntityState 'Player 'Whole)
instance FromJSON (EntityState 'Player 'Whole)

instance ToJSON (EntityState 'Player (Part 'Meta))
instance FromJSON (EntityState 'Player (Part 'Meta))

instance ToJSON (EntityState 'Player (Part 'Attrs))
instance FromJSON (EntityState 'Player (Part 'Attrs))

instance ToJSON (EntityState 'Player (Part 'Modes))
instance FromJSON (EntityState 'Player (Part 'Modes))

instance ToJSON (EntityState 'Player (Part 'Rels))
instance FromJSON (EntityState 'Player (Part 'Rels))

-- EPlayerHand

mkEPlayerHand ::
    EntityState PlayerHand (Part Meta) ->
    EntityState PlayerHand (Part Attrs) ->
    EntityState PlayerHand (Part Modes) ->
    EntityState PlayerHand (Part Rels) ->
    EntityState PlayerHand Whole
mkEPlayerHand = EPlayerHand

mkEPlayerHandAttrs :: [Card] -> Chips -> Int -> Int -> EntityState 'PlayerHand (Part 'Attrs)
mkEPlayerHandAttrs = EPlayerHandAttrs

mkEPlayerHandModes :: SomePlayerHandFSM -> EntityState 'PlayerHand (Part 'Modes)
mkEPlayerHandModes = EPlayerHandModes

mkEPlayerHandRels ::
    EntityRef 'PlayerSpot ->
    EntityRef 'DealerRound ->
    EntityRef 'Player ->
    EntityState 'PlayerHand (Part 'Rels)
mkEPlayerHandRels = EPlayerHandRels

data instance EntityState 'PlayerHand 'Whole = EPlayerHand
    { _phMeta :: EntityState 'PlayerHand (Part 'Meta)
    , _phAttrs :: EntityState 'PlayerHand (Part 'Attrs)
    , _phModes :: EntityState 'PlayerHand (Part 'Modes)
    , _phRels :: EntityState 'PlayerHand (Part 'Rels)
    }
    deriving (Eq, Show, Generic)

newtype instance EntityState 'PlayerHand (Part 'Meta)
    = EPlayerHandMeta (Metadata 'PlayerHand)
    deriving (Eq, Show, Generic)

data instance EntityState 'PlayerHand (Part 'Attrs) = EPlayerHandAttrs
    { _phAttrsHandCards :: [Card]
    , _phAttrsOriginalBet :: Chips
    , _phAttrsSplitDepth :: Int
    , _phAttrsHandIx :: Int
    }
    deriving (Eq, Show, Generic)

data instance EntityState 'PlayerHand (Part 'Modes) = EPlayerHandModes
    { _phFsm :: SomePlayerHandFSM
    }
    deriving (Eq, Show, Generic)

data instance EntityState 'PlayerHand (Part 'Rels) = EPlayerHandRels
    { _phRelsBelongsToPlayerSpot :: EntityRef 'PlayerSpot
    , _phRelsBelongsToDealerRound :: EntityRef 'DealerRound
    , _phRelsOwnedByPlayer :: EntityRef 'Player
    }
    deriving (Eq, Show, Generic)

instance ToJSON (EntityState 'PlayerHand 'Whole)
instance FromJSON (EntityState 'PlayerHand 'Whole)

instance ToJSON (EntityState 'PlayerHand (Part 'Meta))
instance FromJSON (EntityState 'PlayerHand (Part 'Meta))

instance ToJSON (EntityState 'PlayerHand (Part 'Attrs))
instance FromJSON (EntityState 'PlayerHand (Part 'Attrs))

instance ToJSON (EntityState 'PlayerHand (Part 'Modes))
instance FromJSON (EntityState 'PlayerHand (Part 'Modes))

instance ToJSON (EntityState 'PlayerHand (Part 'Rels))
instance FromJSON (EntityState 'PlayerHand (Part 'Rels))

mkEPlayerSpot ::
    EntityState PlayerSpot (Part Meta) ->
    EntityState PlayerSpot (Part Attrs) ->
    EntityState PlayerSpot (Part Modes) ->
    EntityState PlayerSpot (Part Rels) ->
    EntityState PlayerSpot Whole
mkEPlayerSpot = EPlayerSpot

mkEPlayerSpotAttrs :: PlayerSpotIx -> Chips -> EntityState 'PlayerSpot (Part 'Attrs)
mkEPlayerSpotAttrs = EPlayerSpotAttrs

mkEPlayerSpotModes :: SomePlayerSpotFSM -> EntityState 'PlayerSpot (Part 'Modes)
mkEPlayerSpotModes = EPlayerSpotModes

mkEPlayerSpotRels ::
    EntityRef 'Player ->
    EntityRef 'DealerRound ->
    FiniteMap PlayerSpotHandIx (Occupancy (EntityRef 'PlayerHand)) ->
    EntityState 'PlayerSpot (Part 'Rels)
mkEPlayerSpotRels = EPlayerSpotRels

data instance EntityState 'PlayerSpot 'Whole = EPlayerSpot
    { _psMeta :: EntityState 'PlayerSpot (Part 'Meta)
    , _psAttrs :: EntityState 'PlayerSpot (Part 'Attrs)
    , _psModes :: EntityState 'PlayerSpot (Part 'Modes)
    , _psRels :: EntityState 'PlayerSpot (Part 'Rels)
    }
    deriving (Eq, Show, Generic)

newtype instance EntityState 'PlayerSpot (Part 'Meta)
    = EPlayerSpotMeta (Metadata 'PlayerSpot)
    deriving (Eq, Show, Generic)

data instance EntityState 'PlayerSpot (Part 'Attrs) = EPlayerSpotAttrs
    { _psAttrsSpotIndex :: PlayerSpotIx
    , _psAttrsWager :: Chips
    }
    deriving (Eq, Show, Generic)

data instance EntityState 'PlayerSpot (Part 'Modes) = EPlayerSpotModes
    { _psModesPlayerSpot :: SomePlayerSpotFSM
    }
    deriving (Eq, Show, Generic)

data instance EntityState 'PlayerSpot (Part 'Rels) = EPlayerSpotRels
    { _psEntityRelsPlayerId :: EntityRef 'Player
    , _psEntityRelsRoundId :: EntityRef 'DealerRound
    , _psRelsHandOccupancy :: FiniteMap PlayerSpotHandIx (Occupancy (EntityRef 'PlayerHand))
    }
    deriving (Eq, Show, Generic)

instance ToJSON (EntityState 'PlayerSpot 'Whole)
instance FromJSON (EntityState 'PlayerSpot 'Whole)

instance ToJSON (EntityState 'PlayerSpot (Part 'Meta))
instance FromJSON (EntityState 'PlayerSpot (Part 'Meta))

instance ToJSON (EntityState 'PlayerSpot (Part 'Attrs))
instance FromJSON (EntityState 'PlayerSpot (Part 'Attrs))

instance ToJSON (EntityState 'PlayerSpot (Part 'Modes))
instance FromJSON (EntityState 'PlayerSpot (Part 'Modes))

instance ToJSON (EntityState 'PlayerSpot (Part 'Rels))
instance FromJSON (EntityState 'PlayerSpot (Part 'Rels))

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

mkETable ::
    EntityState Table (Part Meta) ->
    EntityState Table (Part Attrs) ->
    EntityState Table (Part Modes) ->
    EntityState Table (Part Rels) ->
    EntityState Table Whole
mkETable = ETable

mkETableAttrs :: String -> Maybe (EntityRef 'DealerRound) -> EntityRef 'Offering -> Chips -> EntityState 'Table (Part 'Attrs)
mkETableAttrs = ETableAttrs

mkETableModes :: SomeTableFSM -> EntityState 'Table (Part 'Modes)
mkETableModes = ETableModes

mkETableRels :: Maybe (EntityRef 'Dealer) -> EntityState 'Table (Part 'Rels)
mkETableRels = ETableRels

data instance EntityState 'Table 'Whole = ETable
    { _tMeta :: EntityState 'Table (Part 'Meta)
    , _tAttrs :: EntityState 'Table (Part 'Attrs)
    , _tModes :: EntityState 'Table (Part 'Modes)
    , _tRels :: EntityState 'Table (Part 'Rels)
    }
    deriving (Eq, Show, Generic)

newtype instance EntityState 'Table (Part 'Meta)
    = ETableMeta (Metadata 'Table)
    deriving (Eq, Show, Generic)

data instance EntityState 'Table (Part 'Attrs) = ETableAttrs
    { _tAttrsName :: String
    , _tAttrsCurrentRound :: Maybe (EntityRef 'DealerRound)
    , _tAttrsOfferingUsed :: EntityRef 'Offering
    , _tAttrsMinBet :: Chips
    }
    deriving (Eq, Show, Generic)

data instance EntityState 'Table (Part 'Modes) = ETableModes
    { _tModesFSM :: SomeTableFSM
    }
    deriving (Eq, Show, Generic)

data instance EntityState 'Table (Part 'Rels) = ETableRels
    { _tRelsManagedByDealer :: Maybe (EntityRef 'Dealer)
    }
    deriving (Eq, Show, Generic)

instance ToJSON (EntityState 'Table 'Whole)
instance FromJSON (EntityState 'Table 'Whole)

instance ToJSON (EntityState 'Table (Part 'Meta))
instance FromJSON (EntityState 'Table (Part 'Meta))

instance ToJSON (EntityState 'Table (Part 'Attrs))
instance FromJSON (EntityState 'Table (Part 'Attrs))

instance ToJSON (EntityState 'Table (Part 'Modes))
instance FromJSON (EntityState 'Table (Part 'Modes))

instance ToJSON (EntityState 'Table (Part 'Rels))
instance FromJSON (EntityState 'Table (Part 'Rels))

-- ETableShoe

mkETableShoe ::
    EntityState TableShoe (Part 'Meta) ->
    EntityState TableShoe (Part 'Attrs) ->
    EntityState TableShoe (Part 'Modes) ->
    EntityState TableShoe (Part 'Rels) ->
    EntityState TableShoe Whole
mkETableShoe = ETableShoe

mkETableShoeAttrs :: [Card] -> Map CardIx CardState -> EntityState 'TableShoe (Part 'Attrs)
mkETableShoeAttrs = ETableShoeAttrs

mkETableShoeModes :: EntityState 'TableShoe (Part 'Modes)
mkETableShoeModes = ETableShoeModes

mkETableShoeRels :: EntityRef 'Table -> EntityState 'TableShoe (Part 'Rels)
mkETableShoeRels = ETableShoeRels

data instance EntityState 'TableShoe 'Whole = ETableShoe
    { _tsMeta :: EntityState 'TableShoe (Part 'Meta)
    , _tsAttrs :: EntityState 'TableShoe (Part 'Attrs)
    , _tsModes :: EntityState 'TableShoe (Part 'Modes)
    , _tsRels :: EntityState 'TableShoe (Part 'Rels)
    }
    deriving (Eq, Show, Generic)

newtype instance EntityState 'TableShoe (Part 'Meta)
    = ETableShoeMeta (Metadata 'TableShoe)
    deriving (Eq, Show, Generic)

data instance EntityState 'TableShoe (Part 'Attrs) = ETableShoeAttrs
    { _tsAttrsCards :: [Card]
    , _tsAttrsCardStates :: Map CardIx CardState
    }
    deriving (Eq, Show, Generic)

data instance EntityState 'TableShoe (Part 'Modes) = ETableShoeModes
    deriving (Eq, Show, Generic)

data instance EntityState 'TableShoe (Part 'Rels) = ETableShoeRels
    { _tsRelsTable :: EntityRef 'Table
    }
    deriving (Eq, Show, Generic)

instance ToJSON (EntityState 'TableShoe 'Whole)
instance FromJSON (EntityState 'TableShoe 'Whole)

instance ToJSON (EntityState 'TableShoe (Part 'Meta))
instance FromJSON (EntityState 'TableShoe (Part 'Meta))

instance ToJSON (EntityState 'TableShoe (Part 'Attrs))
instance FromJSON (EntityState 'TableShoe (Part 'Attrs))

instance ToJSON (EntityState 'TableShoe (Part 'Modes))
instance FromJSON (EntityState 'TableShoe (Part 'Modes))

instance ToJSON (EntityState 'TableShoe (Part 'Rels))
instance FromJSON (EntityState 'TableShoe (Part 'Rels))

type CardIx = Int

data CardState
    = InHand
    | InDiscard
    | Burned
    deriving (Eq, Show, Generic)

instance ToJSON CardState
instance FromJSON CardState

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Pitboss.State.Entity.Capability.Incremental where

-- import Control.Lens (Lens', lens, (%~), (&))
-- import Data.Map.Strict qualified as Map

import Control.Lens ((&))
import Data.Map.Strict qualified as Map
import Pitboss.State.Entity.Capability.Decomposable
import Pitboss.State.Entity.Capability.Replaceable
import Pitboss.State.Entity.Delta
import Pitboss.State.Entity.Entity
import Pitboss.State.Entity.Types.FiniteMap

-- import Pitboss.State.Entity.Types.FiniteMap

-- type family AttrsDelta (k :: EntityKind)
-- type family ModesDelta (k :: EntityKind)
-- type family RelsDelta (k :: EntityKind)
--
-- data family Delta (k :: EntityKind) (f :: DeltaTarget)

data DeltaWrapper k where
    DeltaAttrs :: Delta k (Part 'Attrs) -> DeltaWrapper k
    DeltaModes :: Delta k (Part 'Modes) -> DeltaWrapper k
    DeltaRels :: Delta k (Part 'Rels) -> DeltaWrapper k

class Describable delta where
    type Target delta = target | target -> delta

    previewDelta :: delta -> Target delta -> Target delta
    previewDelta = applyDelta

    applyDelta :: delta -> Target delta -> Target delta

    describeDelta :: delta -> Target delta -> String

class (Incremental delta) => Identifiable delta where
    entityToId :: delta -> Target delta -> Uid

class Incremental target where
    type Applicable target = applicable | applicable -> target

    apply :: Applicable target -> target -> target

    describe :: Applicable target -> target -> String

class IncrementalPart (k :: EntityKind) (part :: EntityStatePart) where
    applyPartDelta :: Delta k ('Part part) -> EntityState k ('Part part) -> EntityState k ('Part part)
    describePartDelta :: Delta k ('Part part) -> EntityState k ('Part part) -> String

-- Dealer
instance Incremental (EntityState 'Dealer (Part 'Attrs)) where
    type Applicable (EntityState 'Dealer (Part 'Attrs)) = Delta 'Dealer (Part 'Attrs)

    apply (DDealerSetName new _) attrs = attrs{_dAttrsName = new}

    describe (DDealerSetName new old) _ = "Set dealer name: " ++ show old ++ " → " ++ show new

instance Incremental (EntityState 'Dealer (Part 'Modes)) where
    type Applicable (EntityState 'Dealer (Part 'Modes)) = Delta 'Dealer (Part 'Modes)

    apply (DDealerSetTableFSM new _) modes = modes{_dModesDealerTable = new}
    apply (DDealerSetRoundFSM new _) modes = modes{_dModesDealerRound = new}
    apply (DDealerSetHandFSM new _) modes = modes{_dModesDealerHand = new}

    describe (DDealerSetTableFSM new old) _ = "Set dealer table FSM: " ++ show old ++ " → " ++ show new
    describe (DDealerSetRoundFSM new old) _ = "Set dealer round FSM: " ++ show old ++ " → " ++ show new
    describe (DDealerSetHandFSM new old) _ = "Set dealer hand FSM: " ++ show old ++ " → " ++ show new

instance Incremental (EntityState 'Dealer (Part 'Rels)) where
    type Applicable (EntityState 'Dealer (Part 'Rels)) = Delta 'Dealer (Part 'Rels)

    apply (DDealerSetActiveTable new _) rels = rels{_dRelsActiveTable = new}
    apply (DDealerSetActiveRound new _) rels = rels{_dRelsActiveRound = new}
    apply (DDealerSetActiveHand new _) rels = rels{_dRelsActiveHand = new}

    describe (DDealerSetActiveTable new old) _ = "Set dealer active table: " ++ show old ++ " → " ++ show new
    describe (DDealerSetActiveRound new old) _ = "Set dealer active round: " ++ show old ++ " → " ++ show new
    describe (DDealerSetActiveHand new old) _ = "Set dealer active hand: " ++ show old ++ " → " ++ show new

instance
    ( Decomposable 'Dealer 'Whole
    , ReplaceableAttrs 'Dealer
    , ReplaceableModes 'Dealer
    , ReplaceableRels 'Dealer
    , Incremental (EntityState 'Dealer (Part 'Attrs))
    , Incremental (EntityState 'Dealer (Part 'Modes))
    , Incremental (EntityState 'Dealer (Part 'Rels))
    ) =>
    Incremental (EntityState 'Dealer 'Whole)
    where
    type Applicable (EntityState 'Dealer 'Whole) = Delta 'Dealer 'Whole

    apply (DDealer attrsDelta modesDelta relsDelta) entity =
        entity
            & replaceAttrs (apply attrsDelta (getAttrs entity))
            & replaceModes (apply modesDelta (getModes entity))
            & replaceRels (apply relsDelta (getRels entity))

    describe (DDealer attrsDelta _ _) entity =
        describe attrsDelta (getAttrs entity)

-- DealerHand
instance Incremental (EntityState 'DealerHand (Part 'Attrs)) where
    type Applicable (EntityState 'DealerHand (Part 'Attrs)) = Delta 'DealerHand (Part 'Attrs)

    apply (DDealerHandPushCard c _) attrs = attrs{_dhAttrsHandCards = c : _dhAttrsHandCards attrs}
    apply (DDealerHandPopCard c _) attrs =
        case _dhAttrsHandCards attrs of
            [] -> attrs
            (x : xs)
                | x == c -> attrs{_dhAttrsHandCards = xs}
                | otherwise ->
                    let (before, after) = break (== c) (_dhAttrsHandCards attrs)
                     in attrs{_dhAttrsHandCards = before ++ drop 1 after}
    apply (DDealerHandSetCards new _) attrs = attrs{_dhAttrsHandCards = new}

    describe (DDealerHandPushCard c _) _ = "Pushed card: " ++ show c
    describe (DDealerHandPopCard c _) _ = "Popped card: " ++ show c
    describe (DDealerHandSetCards new old) _ = "Set dealer hand cards: " ++ show old ++ " → " ++ show new

instance Incremental (EntityState 'DealerHand (Part 'Modes)) where
    type Applicable (EntityState 'DealerHand (Part 'Modes)) = Delta 'DealerHand (Part 'Modes)

    apply (DDealerHandSetFSM new _) modes = modes{_dhModesDealerHand = new}

    describe (DDealerHandSetFSM new old) _ = "Set dealer hand FSM: " ++ show old ++ " → " ++ show new

instance Incremental (EntityState 'DealerHand (Part 'Rels)) where
    type Applicable (EntityState 'DealerHand (Part 'Rels)) = Delta 'DealerHand (Part 'Rels)

    apply (DDealerHandSetRound new _) rels = rels{_dhRelsDealerRound = new}
    apply (DDealerHandSetDealer new _) rels = rels{_dhRelsDealer = new}

    describe (DDealerHandSetRound new old) _ = "Set dealer hand round: " ++ show old ++ " → " ++ show new
    describe (DDealerHandSetDealer new old) _ = "Set dealer hand dealer: " ++ show old ++ " → " ++ show new

instance
    ( Decomposable 'DealerHand 'Whole
    , ReplaceableAttrs 'DealerHand
    , ReplaceableModes 'DealerHand
    , ReplaceableRels 'DealerHand
    , Incremental (EntityState 'DealerHand (Part 'Attrs))
    , Incremental (EntityState 'DealerHand (Part 'Modes))
    , Incremental (EntityState 'DealerHand (Part 'Rels))
    ) =>
    Incremental (EntityState 'DealerHand 'Whole)
    where
    type Applicable (EntityState 'DealerHand 'Whole) = Delta 'DealerHand 'Whole

    apply (DDealerHand attrsDelta modesDelta relsDelta) entity =
        entity
            & replaceAttrs (apply attrsDelta (getAttrs entity))
            & replaceModes (apply modesDelta (getModes entity))
            & replaceRels (apply relsDelta (getRels entity))

    describe (DDealerHand attrsDelta _ _) entity =
        describe attrsDelta (getAttrs entity)

-- DealerRound
instance Incremental (EntityState 'DealerRound (Part 'Attrs)) where
    type Applicable (EntityState 'DealerRound (Part 'Attrs)) = Delta 'DealerRound (Part 'Attrs)

    apply (DDealerRoundSetNumber new _) attrs = attrs{_drAttrsNumber = new}

    describe (DDealerRoundSetNumber new old) _ = "Set dealer round number: " ++ show old ++ " → " ++ show new

instance Incremental (EntityState 'DealerRound (Part 'Modes)) where
    type Applicable (EntityState 'DealerRound (Part 'Modes)) = Delta 'DealerRound (Part 'Modes)

    apply _ modes = modes

    describe _ _ = "No change to dealer round modes"

instance Incremental (EntityState 'DealerRound (Part 'Rels)) where
    type Applicable (EntityState 'DealerRound (Part 'Rels)) = Delta 'DealerRound (Part 'Rels)

    apply (DDealerRoundSetTableShoe new _) rels = rels{_drRelsTableShoeUsed = new}

    describe (DDealerRoundSetTableShoe new old) _ = "Set dealer round table shoe: " ++ show old ++ " → " ++ show new

instance
    ( Decomposable 'DealerRound 'Whole
    , ReplaceableAttrs 'DealerRound
    , ReplaceableModes 'DealerRound
    , ReplaceableRels 'DealerRound
    , Incremental (EntityState 'DealerRound (Part 'Attrs))
    , Incremental (EntityState 'DealerRound (Part 'Modes))
    , Incremental (EntityState 'DealerRound (Part 'Rels))
    ) =>
    Incremental (EntityState 'DealerRound 'Whole)
    where
    type Applicable (EntityState 'DealerRound 'Whole) = Delta 'DealerRound 'Whole

    apply (DDealerRound attrsDelta modesDelta relsDelta) entity =
        entity
            & replaceAttrs (apply attrsDelta (getAttrs entity))
            & replaceModes (apply modesDelta (getModes entity))
            & replaceRels (apply relsDelta (getRels entity))

    describe (DDealerRound attrsDelta _ _) entity =
        describe attrsDelta (getAttrs entity)

-- Offering
instance Incremental (EntityState 'Offering (Part 'Attrs)) where
    type Applicable (EntityState 'Offering (Part 'Attrs)) = Delta 'Offering (Part 'Attrs)

    apply (DOfferingSetOffering new _) _ = EOfferingAttrs new

    describe (DOfferingSetOffering new old) _ = "Set offering: " ++ show old ++ " → " ++ show new

instance Incremental (EntityState 'Offering (Part 'Modes)) where
    type Applicable (EntityState 'Offering (Part 'Modes)) = Delta 'Offering (Part 'Modes)

    apply DOfferingModes modes = modes

    describe DOfferingModes _ = "No change to offering modes"

instance Incremental (EntityState 'Offering (Part 'Rels)) where
    type Applicable (EntityState 'Offering (Part 'Rels)) = Delta 'Offering (Part 'Rels)

    apply DOfferingRels rels = rels

    describe DOfferingRels _ = "No change to offering relations"

instance
    ( Decomposable 'Offering 'Whole
    , ReplaceableAttrs 'Offering
    , ReplaceableModes 'Offering
    , ReplaceableRels 'Offering
    , Incremental (EntityState 'Offering (Part 'Attrs))
    , Incremental (EntityState 'Offering (Part 'Modes))
    , Incremental (EntityState 'Offering (Part 'Rels))
    ) =>
    Incremental (EntityState 'Offering 'Whole)
    where
    type Applicable (EntityState 'Offering 'Whole) = Delta 'Offering 'Whole

    apply (DOffering attrsDelta modesDelta relsDelta) entity =
        entity
            & replaceAttrs (apply attrsDelta (getAttrs entity))
            & replaceModes (apply modesDelta (getModes entity))
            & replaceRels (apply relsDelta (getRels entity))

    describe (DOffering attrsDelta _ _) entity =
        describe attrsDelta (getAttrs entity)

-- Player
instance Incremental (EntityState 'Player (Part 'Attrs)) where
    type Applicable (EntityState 'Player (Part 'Attrs)) = Delta 'Player (Part 'Attrs)

    apply (DPlayerSetName new _) attrs = attrs{_pAttrsName = new}
    apply (DPlayerSetBankroll new _) attrs = attrs{_pAttrsBankroll = new}

    describe (DPlayerSetName new old) _ = "Set player name: " ++ show old ++ " → " ++ show new
    describe (DPlayerSetBankroll new old) _ = "Set player bankroll: " ++ show old ++ " → " ++ show new

instance Incremental (EntityState 'Player (Part 'Modes)) where
    type Applicable (EntityState 'Player (Part 'Modes)) = Delta 'Player (Part 'Modes)

    apply (DPlayerSetTable (Just new) _) modes = modes{_pModesPlayerTable = new}
    apply (DPlayerSetTable Nothing _) modes = modes
    apply (DPlayerSetSpot (Just new) _) modes = modes{_pModesPlayerSpot = new}
    apply (DPlayerSetSpot Nothing _) modes = modes
    apply (DPlayerSetHand (Just new) _) modes = modes{_pModesPlayerHand = new}
    apply (DPlayerSetHand Nothing _) modes = modes

    describe (DPlayerSetTable new old) _ = "Set player table FSM: " ++ show old ++ " → " ++ show new
    describe (DPlayerSetSpot new old) _ = "Set player spot FSM: " ++ show old ++ " → " ++ show new
    describe (DPlayerSetHand new old) _ = "Set player hand FSM: " ++ show old ++ " → " ++ show new

instance Incremental (EntityState 'Player (Part 'Rels)) where
    type Applicable (EntityState 'Player (Part 'Rels)) = Delta 'Player (Part 'Rels)

    apply _ rels = rels

    describe _ _ = "No change to player relations"

instance
    ( Decomposable 'Player 'Whole
    , ReplaceableAttrs 'Player
    , ReplaceableModes 'Player
    , ReplaceableRels 'Player
    , Incremental (EntityState 'Player (Part 'Attrs))
    , Incremental (EntityState 'Player (Part 'Modes))
    , Incremental (EntityState 'Player (Part 'Rels))
    ) =>
    Incremental (EntityState 'Player 'Whole)
    where
    type Applicable (EntityState 'Player 'Whole) = Delta 'Player 'Whole

    apply (DPlayer attrsDelta modesDelta relsDelta) entity =
        entity
            & replaceAttrs (apply attrsDelta (getAttrs entity))
            & replaceModes (apply modesDelta (getModes entity))
            & replaceRels (apply relsDelta (getRels entity))

    describe (DPlayer attrsDelta _ _) entity =
        describe attrsDelta (getAttrs entity)

-- PlayerHand
instance Incremental (EntityState 'PlayerHand (Part 'Attrs)) where
    type Applicable (EntityState 'PlayerHand (Part 'Attrs)) = Delta 'PlayerHand (Part 'Attrs)

    apply (DPlayerHandSetPlayerHandIx new _) attrs = attrs{_phAttrsHandIx = new}
    apply (DPlayerHandSetSplitDepth new _) attrs = attrs{_phAttrsSplitDepth = new}
    apply (DPlayerHandPushCard c _) attrs = attrs{_phAttrsHandCards = c : _phAttrsHandCards attrs}
    apply (DPlayerHandPopCard c _) attrs =
        case _phAttrsHandCards attrs of
            [] -> attrs
            (x : xs)
                | x == c -> attrs{_phAttrsHandCards = xs}
                | otherwise ->
                    let (before, after) = break (== c) (_phAttrsHandCards attrs)
                     in attrs{_phAttrsHandCards = before ++ drop 1 after}
    apply (DPlayerHandSetCards new _) attrs = attrs{_phAttrsHandCards = new}

    describe (DPlayerHandSetPlayerHandIx new old) _ = "Set player hand index: " ++ show old ++ " → " ++ show new
    describe (DPlayerHandSetSplitDepth new old) _ = "Set player hand split depth: " ++ show old ++ " → " ++ show new
    describe (DPlayerHandPushCard c _) _ = "Pushed card: " ++ show c
    describe (DPlayerHandPopCard c _) _ = "Popped card: " ++ show c
    describe (DPlayerHandSetCards new old) _ = "Set player hand cards: " ++ show old ++ " → " ++ show new

instance Incremental (EntityState 'PlayerHand (Part 'Modes)) where
    type Applicable (EntityState 'PlayerHand (Part 'Modes)) = Delta 'PlayerHand (Part 'Modes)

    apply (DPlayerHandSetPlayerHandFSM new _) modes = modes{_phFsm = new}

    describe (DPlayerHandSetPlayerHandFSM new old) _ = "Set player hand FSM: " ++ show old ++ " → " ++ show new

instance Incremental (EntityState 'PlayerHand (Part 'Rels)) where
    type Applicable (EntityState 'PlayerHand (Part 'Rels)) = Delta 'PlayerHand (Part 'Rels)

    apply (DPlayerHandSetPlayerSpot new _) rels = rels{_phRelsBelongsToPlayerSpot = new}

    describe (DPlayerHandSetPlayerSpot new old) _ = "Set player hand spot: " ++ show old ++ " → " ++ show new

instance
    ( Decomposable 'PlayerHand 'Whole
    , ReplaceableAttrs 'PlayerHand
    , ReplaceableModes 'PlayerHand
    , ReplaceableRels 'PlayerHand
    , Incremental (EntityState 'PlayerHand (Part 'Attrs))
    , Incremental (EntityState 'PlayerHand (Part 'Modes))
    , Incremental (EntityState 'PlayerHand (Part 'Rels))
    ) =>
    Incremental (EntityState 'PlayerHand 'Whole)
    where
    type Applicable (EntityState 'PlayerHand 'Whole) = Delta 'PlayerHand 'Whole

    apply (DPlayerHand attrsDelta modesDelta relsDelta) entity =
        entity
            & replaceAttrs (apply attrsDelta (getAttrs entity))
            & replaceModes (apply modesDelta (getModes entity))
            & replaceRels (apply relsDelta (getRels entity))

    describe (DPlayerHand attrsDelta _ _) entity =
        describe attrsDelta (getAttrs entity)

-- PlayerSpot
instance Incremental (EntityState 'PlayerSpot (Part 'Attrs)) where
    type Applicable (EntityState 'PlayerSpot (Part 'Attrs)) = Delta 'PlayerSpot (Part 'Attrs)

    apply (DPlayerSpotSetWager new _) attrs = attrs{_psAttrsWager = new}

    describe (DPlayerSpotSetWager new old) _ = "Set player spot wager: " ++ show old ++ " → " ++ show new

instance Incremental (EntityState 'PlayerSpot (Part 'Modes)) where
    type Applicable (EntityState 'PlayerSpot (Part 'Modes)) = Delta 'PlayerSpot (Part 'Modes)

    apply (DPlayerSpotSetFSM new _) modes = modes{_psModesPlayerSpot = new}

    describe (DPlayerSpotSetFSM new old) _ = "Set player spot FSM: " ++ show old ++ " → " ++ show new

instance Incremental (EntityState 'PlayerSpot (Part 'Rels)) where
    type Applicable (EntityState 'PlayerSpot (Part 'Rels)) = Delta 'PlayerSpot (Part 'Rels)

    apply (DPlayerSpotSetPlayer new _) rels = rels{_psEntityRelsPlayerId = new}
    apply (DPlayerSpotSetRound new _) rels = rels{_psEntityRelsRoundId = new}
    apply (DPlayerSpotSetHandOccupancy (_, _) (k, v)) rels =
        rels{_psRelsHandOccupancy = insertFiniteMap k v (_psRelsHandOccupancy rels)}

    describe (DPlayerSpotSetPlayer new old) _ = "Set player spot player: " ++ show old ++ " → " ++ show new
    describe (DPlayerSpotSetRound new old) _ = "Set player spot round: " ++ show old ++ " → " ++ show new
    describe (DPlayerSpotSetHandOccupancy (_, _) (ix, _)) _ = "Updated hand occupancy at index: " ++ show ix

instance
    ( Decomposable 'PlayerSpot 'Whole
    , ReplaceableAttrs 'PlayerSpot
    , ReplaceableModes 'PlayerSpot
    , ReplaceableRels 'PlayerSpot
    , Incremental (EntityState 'PlayerSpot (Part 'Attrs))
    , Incremental (EntityState 'PlayerSpot (Part 'Modes))
    , Incremental (EntityState 'PlayerSpot (Part 'Rels))
    ) =>
    Incremental (EntityState 'PlayerSpot 'Whole)
    where
    type Applicable (EntityState 'PlayerSpot 'Whole) = Delta 'PlayerSpot 'Whole

    apply (DPlayerSpot attrsDelta modesDelta relsDelta) entity =
        entity
            & replaceAttrs (apply attrsDelta (getAttrs entity))
            & replaceModes (apply modesDelta (getModes entity))
            & replaceRels (apply relsDelta (getRels entity))

    describe (DPlayerSpot attrsDelta _ _) entity =
        describe attrsDelta (getAttrs entity)

-- Table
instance Incremental (EntityState 'Table (Part 'Attrs)) where
    type Applicable (EntityState 'Table (Part 'Attrs)) = Delta 'Table (Part 'Attrs)

    apply (DTableSetName new _) attrs = attrs{_tAttrsName = new}
    apply (DTableSetMinBet new _) attrs = attrs{_tAttrsMinBet = new}
    apply (DTableSetOffering new _) attrs = attrs{_tAttrsOfferingUsed = new}

    describe (DTableSetName new old) _ = "Set table name: " ++ show old ++ " → " ++ show new
    describe (DTableSetMinBet new old) _ = "Set table min bet: " ++ show old ++ " → " ++ show new
    describe (DTableSetOffering new old) _ = "Set table offering: " ++ show old ++ " → " ++ show new

instance Incremental (EntityState 'Table (Part 'Modes)) where
    type Applicable (EntityState 'Table (Part 'Modes)) = Delta 'Table (Part 'Modes)

    apply _ modes = modes

    describe _ _ = "No change to table modes"

instance Incremental (EntityState 'Table (Part 'Rels)) where
    type Applicable (EntityState 'Table (Part 'Rels)) = Delta 'Table (Part 'Rels)

    apply (DTableSetDealer new _) rels = rels{_tRelsManagedByDealer = new}

    describe (DTableSetDealer new old) _ = "Set table dealer: " ++ show old ++ " → " ++ show new

instance
    ( Decomposable 'Table 'Whole
    , ReplaceableAttrs 'Table
    , ReplaceableModes 'Table
    , ReplaceableRels 'Table
    , Incremental (EntityState 'Table (Part 'Attrs))
    , Incremental (EntityState 'Table (Part 'Modes))
    , Incremental (EntityState 'Table (Part 'Rels))
    ) =>
    Incremental (EntityState 'Table 'Whole)
    where
    type Applicable (EntityState 'Table 'Whole) = Delta 'Table 'Whole

    apply (DTable attrsDelta modesDelta relsDelta) entity =
        entity
            & replaceAttrs (apply attrsDelta (getAttrs entity))
            & replaceModes (apply modesDelta (getModes entity))
            & replaceRels (apply relsDelta (getRels entity))

    describe (DTable attrsDelta _ _) entity =
        describe attrsDelta (getAttrs entity)

-- TableShoe
instance Incremental (EntityState 'TableShoe (Part 'Attrs)) where
    type Applicable (EntityState 'TableShoe (Part 'Attrs)) = Delta 'TableShoe (Part 'Attrs)

    apply (DTableShoeSetCardStateMap new _) attrs = attrs{_tsAttrsCardStates = new}
    apply (DTableShoeSetCardFate ix new _) attrs =
        attrs{_tsAttrsCardStates = Map.insert ix new (_tsAttrsCardStates attrs)}

    describe (DTableShoeSetCardStateMap _ _) _ = "Set card state map"
    describe (DTableShoeSetCardFate ix new old) _ = "Set card fate at index " ++ show ix ++ ": " ++ show old ++ " → " ++ show new

instance Incremental (EntityState 'TableShoe (Part 'Modes)) where
    type Applicable (EntityState 'TableShoe (Part 'Modes)) = Delta 'TableShoe (Part 'Modes)

    apply _ modes = modes

    describe _ _ = "No change to table shoe modes"

instance Incremental (EntityState 'TableShoe (Part 'Rels)) where
    type Applicable (EntityState 'TableShoe (Part 'Rels)) = Delta 'TableShoe (Part 'Rels)

    apply (DTableShoeSetTable new _) rels = rels{_tsRelsTable = new}

    describe (DTableShoeSetTable new old) _ = "Set table shoe table: " ++ show old ++ " → " ++ show new

instance
    ( Decomposable 'TableShoe 'Whole
    , ReplaceableAttrs 'TableShoe
    , ReplaceableModes 'TableShoe
    , ReplaceableRels 'TableShoe
    , Incremental (EntityState 'TableShoe (Part 'Attrs))
    , Incremental (EntityState 'TableShoe (Part 'Modes))
    , Incremental (EntityState 'TableShoe (Part 'Rels))
    ) =>
    Incremental (EntityState 'TableShoe 'Whole)
    where
    type Applicable (EntityState 'TableShoe 'Whole) = Delta 'TableShoe 'Whole

    apply (DTableShoe attrsDelta modesDelta relsDelta) entity =
        entity
            & replaceAttrs (apply attrsDelta (getAttrs entity))
            & replaceModes (apply modesDelta (getModes entity))
            & replaceRels (apply relsDelta (getRels entity))

    describe (DTableShoe attrsDelta _ _) entity =
        describe attrsDelta (getAttrs entity)

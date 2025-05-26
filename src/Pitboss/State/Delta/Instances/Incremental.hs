{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Pitboss.State.Delta.Instances.Incremental (
    DeltaWrapper (..),
    Incremental (..),
    IncrementalWithWitness (..),
    mkEntityRef,
) where

import Data.Map.Strict qualified as Map
import Pitboss.State.Delta.Types
import Pitboss.State.Entity.Types
import Pitboss.State.Types.Core
import Pitboss.State.Types.FiniteMap

data DeltaWrapper k where
    DeltaAttrs :: Delta k (PartialUpdate 'Attrs) -> DeltaWrapper k
    DeltaModes :: Delta k (PartialUpdate 'Modes) -> DeltaWrapper k
    DeltaRels :: Delta k (PartialUpdate 'Rels) -> DeltaWrapper k

class Incremental target where
    type Applicable target = applicable | applicable -> target

    apply :: Applicable target -> target -> target

    describe :: Applicable target -> target -> String

data PartWitness (s :: EntityStatePart) where
    AttrsWitness :: PartWitness 'Attrs
    ModesWitness :: PartWitness 'Modes
    RelsWitness :: PartWitness 'Rels

class IncrementalWithWitness k where
    applyWithWitness ::
        PartWitness s ->
        Delta k (PartialUpdate s) ->
        EntityState k (PartialUpdate s) ->
        EntityState k (PartialUpdate s)

mkEntityRef :: Tick -> EntityId k -> EntityRef k
mkEntityRef tick entityId = EntityRef (Uid (tick, entityId))

-- Dealer
instance Incremental (EntityState 'Dealer (PartialUpdate 'Attrs)) where
    type Applicable (EntityState 'Dealer (PartialUpdate 'Attrs)) = Delta 'Dealer (PartialUpdate 'Attrs)

    apply (DDealerSetName new _) attrs = attrs{_dAttrsName = new}

    describe (DDealerSetName new old) _ = "Set dealer name: " ++ show old ++ " → " ++ show new

instance Incremental (EntityState 'Dealer (PartialUpdate 'Modes)) where
    type Applicable (EntityState 'Dealer (PartialUpdate 'Modes)) = Delta 'Dealer (PartialUpdate 'Modes)

    apply (DDealerSetTableFSM new _) modes = modes{_dModesDealerTable = new}
    apply (DDealerSetRoundFSM new _) modes = modes{_dModesDealerRound = new}
    apply (DDealerSetHandFSM new _) modes = modes{_dModesDealerHand = new}

    describe (DDealerSetTableFSM new old) _ = "Set dealer table FSM: " ++ show old ++ " → " ++ show new
    describe (DDealerSetRoundFSM new old) _ = "Set dealer round FSM: " ++ show old ++ " → " ++ show new
    describe (DDealerSetHandFSM new old) _ = "Set dealer hand FSM: " ++ show old ++ " → " ++ show new

instance Incremental (EntityState 'Dealer (PartialUpdate 'Rels)) where
    type Applicable (EntityState 'Dealer (PartialUpdate 'Rels)) = Delta 'Dealer (PartialUpdate 'Rels)

    apply (DDealerSetActiveTable new _) rels = rels{_dRelsActiveTable = new}
    apply (DDealerSetActiveRound new _) rels = rels{_dRelsActiveRound = new}
    apply (DDealerSetActiveHand new _) rels = rels{_dRelsActiveHand = new}

    describe (DDealerSetActiveTable new old) _ = "Set dealer active table: " ++ show old ++ " → " ++ show new
    describe (DDealerSetActiveRound new old) _ = "Set dealer active round: " ++ show old ++ " → " ++ show new
    describe (DDealerSetActiveHand new old) _ = "Set dealer active hand: " ++ show old ++ " → " ++ show new

-- DealerHand
instance Incremental (EntityState 'DealerHand (PartialUpdate 'Attrs)) where
    type Applicable (EntityState 'DealerHand (PartialUpdate 'Attrs)) = Delta 'DealerHand (PartialUpdate 'Attrs)

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

instance Incremental (EntityState 'DealerHand (PartialUpdate 'Modes)) where
    type Applicable (EntityState 'DealerHand (PartialUpdate 'Modes)) = Delta 'DealerHand (PartialUpdate 'Modes)

    apply (DDealerHandSetFSM new _) modes = modes{_dhModesDealerHand = new}

    describe (DDealerHandSetFSM new old) _ = "Set dealer hand FSM: " ++ show old ++ " → " ++ show new

instance Incremental (EntityState 'DealerHand (PartialUpdate 'Rels)) where
    type Applicable (EntityState 'DealerHand (PartialUpdate 'Rels)) = Delta 'DealerHand (PartialUpdate 'Rels)

    apply (DDealerHandSetRound new _) rels = rels{_dhRelsDealerRound = new}
    apply (DDealerHandSetDealer new _) rels = rels{_dhRelsDealer = new}

    describe (DDealerHandSetRound new old) _ = "Set dealer hand round: " ++ show old ++ " → " ++ show new
    describe (DDealerHandSetDealer new old) _ = "Set dealer hand dealer: " ++ show old ++ " → " ++ show new

-- DealerRound
instance Incremental (EntityState 'DealerRound (PartialUpdate 'Attrs)) where
    type Applicable (EntityState 'DealerRound (PartialUpdate 'Attrs)) = Delta 'DealerRound (PartialUpdate 'Attrs)

    apply (DDealerRoundSetNumber new _) attrs = attrs{_drAttrsNumber = new}

    describe (DDealerRoundSetNumber new old) _ = "Set dealer round number: " ++ show old ++ " → " ++ show new

instance Incremental (EntityState 'DealerRound (PartialUpdate 'Modes)) where
    type Applicable (EntityState 'DealerRound (PartialUpdate 'Modes)) = Delta 'DealerRound (PartialUpdate 'Modes)

    apply _ modes = modes

    describe _ _ = "No change to dealer round modes"

instance Incremental (EntityState 'DealerRound (PartialUpdate 'Rels)) where
    type Applicable (EntityState 'DealerRound (PartialUpdate 'Rels)) = Delta 'DealerRound (PartialUpdate 'Rels)

    apply (DDealerRoundSetTableShoe new _) rels = rels{_drRelsTableShoeUsed = new}

    describe (DDealerRoundSetTableShoe new old) _ = "Set dealer round table shoe: " ++ show old ++ " → " ++ show new

-- Offering
instance Incremental (EntityState 'Offering (PartialUpdate 'Attrs)) where
    type Applicable (EntityState 'Offering (PartialUpdate 'Attrs)) = Delta 'Offering (PartialUpdate 'Attrs)

    apply (DOfferingSetOffering new _) _ = EOfferingAttrs new

    describe (DOfferingSetOffering new old) _ = "Set offering: " ++ show old ++ " → " ++ show new

instance Incremental (EntityState 'Offering (PartialUpdate 'Modes)) where
    type Applicable (EntityState 'Offering (PartialUpdate 'Modes)) = Delta 'Offering (PartialUpdate 'Modes)

    apply DOfferingModes modes = modes

    describe DOfferingModes _ = "No change to offering modes"

instance Incremental (EntityState 'Offering (PartialUpdate 'Rels)) where
    type Applicable (EntityState 'Offering (PartialUpdate 'Rels)) = Delta 'Offering (PartialUpdate 'Rels)

    apply DOfferingRels rels = rels

    describe DOfferingRels _ = "No change to offering relations"

-- Player
instance Incremental (EntityState 'Player (PartialUpdate 'Attrs)) where
    type Applicable (EntityState 'Player (PartialUpdate 'Attrs)) = Delta 'Player (PartialUpdate 'Attrs)

    apply (DPlayerSetName new _) attrs = attrs{_pAttrsName = new}
    apply (DPlayerSetBankroll new _) attrs = attrs{_pAttrsBankroll = new}

    describe (DPlayerSetName new old) _ = "Set player name: " ++ show old ++ " → " ++ show new
    describe (DPlayerSetBankroll new old) _ = "Set player bankroll: " ++ show old ++ " → " ++ show new

instance Incremental (EntityState 'Player (PartialUpdate 'Modes)) where
    type Applicable (EntityState 'Player (PartialUpdate 'Modes)) = Delta 'Player (PartialUpdate 'Modes)

    apply (DPlayerSetTable (Just new) _) modes = modes{_pModesPlayerTable = new}
    apply (DPlayerSetTable Nothing _) modes = modes
    apply (DPlayerSetSpot (Just new) _) modes = modes{_pModesPlayerSpot = new}
    apply (DPlayerSetSpot Nothing _) modes = modes
    apply (DPlayerSetHand (Just new) _) modes = modes{_pModesPlayerHand = new}
    apply (DPlayerSetHand Nothing _) modes = modes

    describe (DPlayerSetTable new old) _ = "Set player table FSM: " ++ show old ++ " → " ++ show new
    describe (DPlayerSetSpot new old) _ = "Set player spot FSM: " ++ show old ++ " → " ++ show new
    describe (DPlayerSetHand new old) _ = "Set player hand FSM: " ++ show old ++ " → " ++ show new

instance Incremental (EntityState 'Player (PartialUpdate 'Rels)) where
    type Applicable (EntityState 'Player (PartialUpdate 'Rels)) = Delta 'Player (PartialUpdate 'Rels)

    apply _ rels = rels

    describe _ _ = "No change to player relations"

-- PlayerHand
instance Incremental (EntityState 'PlayerHand (PartialUpdate 'Attrs)) where
    type Applicable (EntityState 'PlayerHand (PartialUpdate 'Attrs)) = Delta 'PlayerHand (PartialUpdate 'Attrs)

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

instance Incremental (EntityState 'PlayerHand (PartialUpdate 'Modes)) where
    type Applicable (EntityState 'PlayerHand (PartialUpdate 'Modes)) = Delta 'PlayerHand (PartialUpdate 'Modes)

    apply (DPlayerHandSetPlayerHandFSM new _) modes = modes{_phFsm = new}

    describe (DPlayerHandSetPlayerHandFSM new old) _ = "Set player hand FSM: " ++ show old ++ " → " ++ show new

instance Incremental (EntityState 'PlayerHand (PartialUpdate 'Rels)) where
    type Applicable (EntityState 'PlayerHand (PartialUpdate 'Rels)) = Delta 'PlayerHand (PartialUpdate 'Rels)

    apply (DPlayerHandSetPlayerSpot new _) rels = rels{_phRelsBelongsToPlayerSpot = new}

    describe (DPlayerHandSetPlayerSpot new old) _ = "Set player hand spot: " ++ show old ++ " → " ++ show new

-- PlayerSpot
instance Incremental (EntityState 'PlayerSpot (PartialUpdate 'Attrs)) where
    type Applicable (EntityState 'PlayerSpot (PartialUpdate 'Attrs)) = Delta 'PlayerSpot (PartialUpdate 'Attrs)

    apply (DPlayerSpotSetWager new _) attrs = attrs{_psAttrsWager = new}

    describe (DPlayerSpotSetWager new old) _ = "Set player spot wager: " ++ show old ++ " → " ++ show new

instance Incremental (EntityState 'PlayerSpot (PartialUpdate 'Modes)) where
    type Applicable (EntityState 'PlayerSpot (PartialUpdate 'Modes)) = Delta 'PlayerSpot (PartialUpdate 'Modes)

    apply (DPlayerSpotSetFSM new _) modes = modes{_psModesPlayerSpot = new}

    describe (DPlayerSpotSetFSM new old) _ = "Set player spot FSM: " ++ show old ++ " → " ++ show new

instance Incremental (EntityState 'PlayerSpot (PartialUpdate 'Rels)) where
    type Applicable (EntityState 'PlayerSpot (PartialUpdate 'Rels)) = Delta 'PlayerSpot (PartialUpdate 'Rels)

    apply (DPlayerSpotSetPlayer new _) rels = rels{_psEntityRelsPlayerId = new}
    apply (DPlayerSpotSetRound new _) rels = rels{_psEntityRelsRoundId = new}
    apply (DPlayerSpotSetHandOccupancy (_, _) (k, v)) rels =
        rels{_psRelsHandOccupancy = insertFiniteMap k v (_psRelsHandOccupancy rels)}

    describe (DPlayerSpotSetPlayer new old) _ = "Set player spot player: " ++ show old ++ " → " ++ show new
    describe (DPlayerSpotSetRound new old) _ = "Set player spot round: " ++ show old ++ " → " ++ show new
    describe (DPlayerSpotSetHandOccupancy (_, _) (ix, _)) _ = "Updated hand occupancy at index: " ++ show ix

-- Table
instance Incremental (EntityState 'Table (PartialUpdate 'Attrs)) where
    type Applicable (EntityState 'Table (PartialUpdate 'Attrs)) = Delta 'Table (PartialUpdate 'Attrs)

    apply (DTableSetName new _) attrs = attrs{_tAttrsName = new}
    apply (DTableSetMinBet new _) attrs = attrs{_tAttrsMinBet = new}
    apply (DTableSetOffering new _) attrs = attrs{_tAttrsOfferingUsed = new}

    describe (DTableSetName new old) _ = "Set table name: " ++ show old ++ " → " ++ show new
    describe (DTableSetMinBet new old) _ = "Set table min bet: " ++ show old ++ " → " ++ show new
    describe (DTableSetOffering new old) _ = "Set table offering: " ++ show old ++ " → " ++ show new

instance Incremental (EntityState 'Table (PartialUpdate 'Modes)) where
    type Applicable (EntityState 'Table (PartialUpdate 'Modes)) = Delta 'Table (PartialUpdate 'Modes)

    apply _ modes = modes

    describe _ _ = "No change to table modes"

instance Incremental (EntityState 'Table (PartialUpdate 'Rels)) where
    type Applicable (EntityState 'Table (PartialUpdate 'Rels)) = Delta 'Table (PartialUpdate 'Rels)

    apply (DTableSetDealer new _) rels = rels{_tRelsManagedByDealer = new}

    describe (DTableSetDealer new old) _ = "Set table dealer: " ++ show old ++ " → " ++ show new

-- TableShoe
instance Incremental (EntityState 'TableShoe (PartialUpdate 'Attrs)) where
    type Applicable (EntityState 'TableShoe (PartialUpdate 'Attrs)) = Delta 'TableShoe (PartialUpdate 'Attrs)

    apply (DTableShoeSetCardStateMap new _) attrs = attrs{_tsAttrsCardStates = new}
    apply (DTableShoeSetCardFate ix new _) attrs =
        attrs{_tsAttrsCardStates = Map.insert ix new (_tsAttrsCardStates attrs)}

    describe (DTableShoeSetCardStateMap _ _) _ = "Set card state map"
    describe (DTableShoeSetCardFate ix new old) _ = "Set card fate at index " ++ show ix ++ ": " ++ show old ++ " → " ++ show new

instance Incremental (EntityState 'TableShoe (PartialUpdate 'Modes)) where
    type Applicable (EntityState 'TableShoe (PartialUpdate 'Modes)) = Delta 'TableShoe (PartialUpdate 'Modes)

    apply _ modes = modes

    describe _ _ = "No change to table shoe modes"

instance Incremental (EntityState 'TableShoe (PartialUpdate 'Rels)) where
    type Applicable (EntityState 'TableShoe (PartialUpdate 'Rels)) = Delta 'TableShoe (PartialUpdate 'Rels)

    apply (DTableShoeSetTable new _) rels = rels{_tsRelsTable = new}

    describe (DTableShoeSetTable new old) _ = "Set table shoe table: " ++ show old ++ " → " ++ show new

instance IncrementalWithWitness 'Dealer where
    applyWithWitness AttrsWitness = apply
    applyWithWitness ModesWitness = apply
    applyWithWitness RelsWitness = apply

instance IncrementalWithWitness 'Player where
    applyWithWitness AttrsWitness = apply
    applyWithWitness ModesWitness = apply
    applyWithWitness RelsWitness = apply

instance IncrementalWithWitness 'DealerHand where
    applyWithWitness AttrsWitness = apply
    applyWithWitness ModesWitness = apply
    applyWithWitness RelsWitness = apply

instance IncrementalWithWitness 'DealerRound where
    applyWithWitness AttrsWitness = apply
    applyWithWitness ModesWitness = apply
    applyWithWitness RelsWitness = apply

instance IncrementalWithWitness 'Offering where
    applyWithWitness AttrsWitness = apply
    applyWithWitness ModesWitness = apply
    applyWithWitness RelsWitness = apply

instance IncrementalWithWitness 'PlayerHand where
    applyWithWitness AttrsWitness = apply
    applyWithWitness ModesWitness = apply
    applyWithWitness RelsWitness = apply

instance IncrementalWithWitness 'PlayerSpot where
    applyWithWitness AttrsWitness = apply
    applyWithWitness ModesWitness = apply
    applyWithWitness RelsWitness = apply

instance IncrementalWithWitness 'Table where
    applyWithWitness AttrsWitness = apply
    applyWithWitness ModesWitness = apply
    applyWithWitness RelsWitness = apply

instance IncrementalWithWitness 'TableShoe where
    applyWithWitness AttrsWitness = apply
    applyWithWitness ModesWitness = apply
    applyWithWitness RelsWitness = apply

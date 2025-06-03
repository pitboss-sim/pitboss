{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Pitboss.Causality.Delta.Incremental where

import Data.Map.Strict qualified as Map
import Pitboss.Causality.Delta.Types
import Pitboss.Causality.Entity.Types
import Pitboss.Causality.Types.Core
import Pitboss.Causality.Types.FiniteMap

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
        EntityState k ->
        EntityState k

mkEntityRef :: Tick -> EntityId k -> EntityRef k
mkEntityRef tick entityId = EntityRef (Uid (tick, entityId))

-- EBout
instance Incremental BoutAttrs where
    type Applicable BoutAttrs = Delta 'Bout (PartialUpdate 'Attrs)

    apply (DBoutSetOutcome new _) attrs = attrs{_boutAttrsOutcome = new}

    describe (DBoutSetOutcome new old) _ = "Set bout outcome: " ++ show old ++ " → " ++ show new

instance Incremental BoutModes where
    type Applicable BoutModes = Delta 'Bout (PartialUpdate 'Modes)

    apply (DBoutSetFSM new _) modes = modes{_boutModesFSM = new}

    describe (DBoutSetFSM new old) _ = "Set bout FSM: " ++ show old ++ " → " ++ show new

instance Incremental BoutRels where
    type Applicable BoutRels = Delta 'Bout (PartialUpdate 'Rels)

    apply (DBoutSetPlayerHand new _) rels = rels{_boutRelsPlayerHand = new}
    apply (DBoutSetDealerHand new _) rels = rels{_boutRelsDealerHand = new}
    apply (DBoutSetTableShoe new _) rels = rels{_boutRelsTableShoe = new}
    apply (DBoutSetTable new _) rels = rels{_boutRelsTable = new}
    apply (DBoutSetDealerRound new _) rels = rels{_boutRelsDealerRound = new}

    describe (DBoutSetPlayerHand new old) _ = "Set bout player hand: " ++ show old ++ " → " ++ show new
    describe (DBoutSetDealerHand new old) _ = "Set bout dealer hand: " ++ show old ++ " → " ++ show new
    describe (DBoutSetTableShoe new old) _ = "Set bout table shoe: " ++ show old ++ " → " ++ show new
    describe (DBoutSetTable new old) _ = "Set bout table: " ++ show old ++ " → " ++ show new
    describe (DBoutSetDealerRound new old) _ = "Set bout dealer round: " ++ show old ++ " → " ++ show new

instance IncrementalWithWitness 'Bout where
    applyWithWitness AttrsWitness delta (EBout attrs modes rels) =
        EBout (apply delta attrs) modes rels
    applyWithWitness ModesWitness delta (EBout attrs modes rels) =
        EBout attrs (apply delta modes) rels
    applyWithWitness RelsWitness delta (EBout attrs modes rels) =
        EBout attrs modes (apply delta rels)

-- EDealer
instance Incremental DealerAttrs where
    type Applicable DealerAttrs = Delta 'Dealer (PartialUpdate 'Attrs)

    apply (DDealerSetName new _) attrs = attrs{_dAttrsName = new}

    describe (DDealerSetName new old) _ = "Set dealer name: " ++ show old ++ " → " ++ show new

instance Incremental DealerModes where
    type Applicable DealerModes = Delta 'Dealer (PartialUpdate 'Modes)

    apply (DDealerSetTableFSM new _) modes = modes{_dModesDealerTable = new}
    apply (DDealerSetRoundFSM new _) modes = modes{_dModesDealerRound = new}
    apply (DDealerSetHandFSM new _) modes = modes{_dModesDealerHand = new}

    describe (DDealerSetTableFSM new old) _ = "Set dealer table FSM: " ++ show old ++ " → " ++ show new
    describe (DDealerSetRoundFSM new old) _ = "Set dealer round FSM: " ++ show old ++ " → " ++ show new
    describe (DDealerSetHandFSM new old) _ = "Set dealer hand FSM: " ++ show old ++ " → " ++ show new

instance Incremental DealerRels where
    type Applicable DealerRels = Delta 'Dealer (PartialUpdate 'Rels)

    apply (DDealerSetActiveTable new _) rels = rels{_dRelsActiveTable = new}
    apply (DDealerSetActiveRound new _) rels = rels{_dRelsActiveRound = new}
    apply (DDealerSetActiveHand new _) rels = rels{_dRelsActiveHand = new}

    describe (DDealerSetActiveTable new old) _ = "Set dealer active table: " ++ show old ++ " → " ++ show new
    describe (DDealerSetActiveRound new old) _ = "Set dealer active round: " ++ show old ++ " → " ++ show new
    describe (DDealerSetActiveHand new old) _ = "Set dealer active hand: " ++ show old ++ " → " ++ show new

-- EDealerHand
instance Incremental DealerHandAttrs where
    type Applicable DealerHandAttrs = Delta 'DealerHand (PartialUpdate 'Attrs)

    apply (DDealerHandSetHand _ newHand) attrs = attrs{_dhAttrsHand = newHand}

    describe (DDealerHandSetHand newHand oldHand) _ = "Set dealer hand: " ++ show oldHand ++ " → " ++ show newHand

instance Incremental DealerHandModes where
    type Applicable DealerHandModes = Delta 'DealerHand (PartialUpdate 'Modes)

    apply (DDealerHandSetFSM new _) modes = modes{_dhModesDealerHand = new}

    describe (DDealerHandSetFSM new old) _ = "Set dealer hand FSM: " ++ show old ++ " → " ++ show new

instance Incremental DealerHandRels where
    type Applicable DealerHandRels = Delta 'DealerHand (PartialUpdate 'Rels)

    apply (DDealerHandSetRound new _) rels = rels{_dhRelsDealerRound = new}
    apply (DDealerHandSetDealer new _) rels = rels{_dhRelsDealer = new}

    describe (DDealerHandSetRound new old) _ = "Set dealer hand round: " ++ show old ++ " → " ++ show new
    describe (DDealerHandSetDealer new old) _ = "Set dealer hand dealer: " ++ show old ++ " → " ++ show new

-- EDealerRound
instance Incremental DealerRoundAttrs where
    type Applicable DealerRoundAttrs = Delta 'DealerRound (PartialUpdate 'Attrs)

    apply (DDealerRoundSetNumber new _) attrs = attrs{_drAttrsNumber = new}

    describe (DDealerRoundSetNumber new old) _ = "Set dealer round number: " ++ show old ++ " → " ++ show new

instance Incremental DealerRoundModes where
    type Applicable DealerRoundModes = Delta 'DealerRound (PartialUpdate 'Modes)

    apply _ modes = modes

    describe _ _ = "No change to dealer round modes"

instance Incremental DealerRoundRels where
    type Applicable DealerRoundRels = Delta 'DealerRound (PartialUpdate 'Rels)

    apply (DDealerRoundSetTableShoe new _) rels = rels{_drRelsTableShoeUsed = new}

    describe (DDealerRoundSetTableShoe new old) _ = "Set dealer round table shoe: " ++ show old ++ " → " ++ show new

-- EPlayer
instance Incremental PlayerAttrs where
    type Applicable PlayerAttrs = Delta 'Player (PartialUpdate 'Attrs)

    apply (DPlayerSetName new _) attrs = attrs{_pAttrsName = new}
    apply (DPlayerSetBankroll new _) attrs = attrs{_pAttrsBankroll = new}

    describe (DPlayerSetName new old) _ = "Set player name: " ++ show old ++ " → " ++ show new
    describe (DPlayerSetBankroll new old) _ = "Set player bankroll: " ++ show old ++ " → " ++ show new

instance Incremental PlayerModes where
    type Applicable PlayerModes = Delta 'Player (PartialUpdate 'Modes)

    apply (DPlayerSetTable (Just new) _) modes = modes{_pModesPlayerTable = new}
    apply (DPlayerSetTable Nothing _) modes = modes
    apply (DPlayerSetSpot (Just new) _) modes = modes{_pModesPlayerSpot = new}
    apply (DPlayerSetSpot Nothing _) modes = modes
    apply (DPlayerSetHand (Just new) _) modes = modes{_pModesPlayerHand = new}
    apply (DPlayerSetHand Nothing _) modes = modes

    describe (DPlayerSetTable new old) _ = "Set player table FSM: " ++ show old ++ " → " ++ show new
    describe (DPlayerSetSpot new old) _ = "Set player spot FSM: " ++ show old ++ " → " ++ show new
    describe (DPlayerSetHand new old) _ = "Set player hand FSM: " ++ show old ++ " → " ++ show new

instance Incremental PlayerRels where
    type Applicable PlayerRels = Delta 'Player (PartialUpdate 'Rels)

    apply _ rels = rels

    describe _ _ = "No change to player relations"

-- EPlayerHand
instance Incremental PlayerHandAttrs where
    type Applicable PlayerHandAttrs = Delta 'PlayerHand (PartialUpdate 'Attrs)

    apply (DPlayerHandSetPlayerHandIx new _) attrs = attrs{_phAttrsHandIx = new}
    apply (DPlayerHandSetSplitDepth new _) attrs = attrs{_phAttrsSplitDepth = new}
    apply (DPlayerHandSetHand _ newHand) attrs = attrs{_phAttrsHand = newHand}

    describe (DPlayerHandSetPlayerHandIx new old) _ = "Set player hand index: " ++ show old ++ " → " ++ show new
    describe (DPlayerHandSetSplitDepth new old) _ = "Set player hand split depth: " ++ show old ++ " → " ++ show new
    describe (DPlayerHandSetHand newHand oldHand) _ = "Set player hand: " ++ show oldHand ++ " → " ++ show newHand

instance Incremental PlayerHandModes where
    type Applicable PlayerHandModes = Delta 'PlayerHand (PartialUpdate 'Modes)

    apply (DPlayerHandSetPlayerHandFSM new _) modes = modes{_phFsm = new}

    describe (DPlayerHandSetPlayerHandFSM new old) _ = "Set player hand FSM: " ++ show old ++ " → " ++ show new

instance Incremental PlayerHandRels where
    type Applicable PlayerHandRels = Delta 'PlayerHand (PartialUpdate 'Rels)

    apply (DPlayerHandSetPlayerSpot new _) rels = rels{_phRelsBelongsToPlayerSpot = new}

    describe (DPlayerHandSetPlayerSpot new old) _ = "Set player hand spot: " ++ show old ++ " → " ++ show new

-- EPlayerSpot
instance Incremental PlayerSpotAttrs where
    type Applicable PlayerSpotAttrs = Delta 'PlayerSpot (PartialUpdate 'Attrs)

    apply (DPlayerSpotSetWager new _) attrs = attrs{_psAttrsWager = new}

    describe (DPlayerSpotSetWager new old) _ = "Set player spot wager: " ++ show old ++ " → " ++ show new

instance Incremental PlayerSpotModes where
    type Applicable PlayerSpotModes = Delta 'PlayerSpot (PartialUpdate 'Modes)

    apply (DPlayerSpotSetFSM new _) modes = modes{_psModesPlayerSpot = new}

    describe (DPlayerSpotSetFSM new old) _ = "Set player spot FSM: " ++ show old ++ " → " ++ show new

instance Incremental PlayerSpotRels where
    type Applicable PlayerSpotRels = Delta 'PlayerSpot (PartialUpdate 'Rels)

    apply (DPlayerSpotSetPlayer new _) rels = rels{_psEntityRelsPlayerId = new}
    apply (DPlayerSpotSetRound new _) rels = rels{_psEntityRelsRoundId = new}
    apply (DPlayerSpotSetHandOccupancy (_, _) (k, v)) rels =
        rels{_psRelsHandOccupancy = insertFiniteMap k v (_psRelsHandOccupancy rels)}

    describe (DPlayerSpotSetPlayer new old) _ = "Set player spot player: " ++ show old ++ " → " ++ show new
    describe (DPlayerSpotSetRound new old) _ = "Set player spot round: " ++ show old ++ " → " ++ show new
    describe (DPlayerSpotSetHandOccupancy (_, _) (ix, _)) _ = "Updated hand occupancy at index: " ++ show ix

-- ETable
instance Incremental TableAttrs where
    type Applicable TableAttrs = Delta 'Table (PartialUpdate 'Attrs)

    apply (DTableSetName new _) attrs = attrs{_tAttrsName = new}
    apply (DTableSetOffering new _) attrs = attrs{_tAttrsOffering = new}

    describe (DTableSetName new old) _ = "Set table name: " ++ show old ++ " → " ++ show new
    describe (DTableSetOffering new old) _ = "Set table offering: " ++ show old ++ " → " ++ show new

instance Incremental TableModes where
    type Applicable TableModes = Delta 'Table (PartialUpdate 'Modes)

    apply _ modes = modes

    describe _ _ = "No change to table modes"

instance Incremental TableRels where
    type Applicable TableRels = Delta 'Table (PartialUpdate 'Rels)

    apply (DTableSetDealer new _) rels = rels{_tRelsManagedByDealer = new}

    describe (DTableSetDealer new old) _ = "Set table dealer: " ++ show old ++ " → " ++ show new

-- ETableShoe
instance Incremental TableShoeAttrs where
    type Applicable TableShoeAttrs = Delta 'TableShoe (PartialUpdate 'Attrs)

    apply (DTableShoeSetCardStateMap new _) attrs = attrs{_tsAttrsCardStates = new}
    apply (DTableShoeSetCardFate ix new _) attrs =
        attrs{_tsAttrsCardStates = Map.insert ix new (_tsAttrsCardStates attrs)}

    describe (DTableShoeSetCardStateMap _ _) _ = "Set card state map"
    describe (DTableShoeSetCardFate ix new old) _ = "Set card fate at index " ++ show ix ++ ": " ++ show old ++ " → " ++ show new

instance Incremental TableShoeModes where
    type Applicable TableShoeModes = Delta 'TableShoe (PartialUpdate 'Modes)

    apply _ modes = modes

    describe _ _ = "No change to table shoe modes"

instance Incremental TableShoeRels where
    type Applicable TableShoeRels = Delta 'TableShoe (PartialUpdate 'Rels)

    apply (DTableShoeSetTable new _) rels = rels{_tsRelsTable = new}

    describe (DTableShoeSetTable new old) _ = "Set table shoe table: " ++ show old ++ " → " ++ show new

instance IncrementalWithWitness 'Dealer where
    applyWithWitness AttrsWitness delta (EDealer attrs modes rels) =
        EDealer (apply delta attrs) modes rels
    applyWithWitness ModesWitness delta (EDealer attrs modes rels) =
        EDealer attrs (apply delta modes) rels
    applyWithWitness RelsWitness delta (EDealer attrs modes rels) =
        EDealer attrs modes (apply delta rels)

instance IncrementalWithWitness 'Player where
    applyWithWitness AttrsWitness delta (EPlayer attrs modes rels) =
        EPlayer (apply delta attrs) modes rels
    applyWithWitness ModesWitness delta (EPlayer attrs modes rels) =
        EPlayer attrs (apply delta modes) rels
    applyWithWitness RelsWitness delta (EPlayer attrs modes rels) =
        EPlayer attrs modes (apply delta rels)

instance IncrementalWithWitness 'DealerHand where
    applyWithWitness AttrsWitness delta (EDealerHand attrs modes rels) =
        EDealerHand (apply delta attrs) modes rels
    applyWithWitness ModesWitness delta (EDealerHand attrs modes rels) =
        EDealerHand attrs (apply delta modes) rels
    applyWithWitness RelsWitness delta (EDealerHand attrs modes rels) =
        EDealerHand attrs modes (apply delta rels)

instance IncrementalWithWitness 'DealerRound where
    applyWithWitness AttrsWitness delta (EDealerRound attrs modes rels) =
        EDealerRound (apply delta attrs) modes rels
    applyWithWitness ModesWitness delta (EDealerRound attrs modes rels) =
        EDealerRound attrs (apply delta modes) rels
    applyWithWitness RelsWitness delta (EDealerRound attrs modes rels) =
        EDealerRound attrs modes (apply delta rels)

instance IncrementalWithWitness 'PlayerHand where
    applyWithWitness AttrsWitness delta (EPlayerHand attrs modes rels) =
        EPlayerHand (apply delta attrs) modes rels
    applyWithWitness ModesWitness delta (EPlayerHand attrs modes rels) =
        EPlayerHand attrs (apply delta modes) rels
    applyWithWitness RelsWitness delta (EPlayerHand attrs modes rels) =
        EPlayerHand attrs modes (apply delta rels)

instance IncrementalWithWitness 'PlayerSpot where
    applyWithWitness AttrsWitness delta (EPlayerSpot attrs modes rels) =
        EPlayerSpot (apply delta attrs) modes rels
    applyWithWitness ModesWitness delta (EPlayerSpot attrs modes rels) =
        EPlayerSpot attrs (apply delta modes) rels
    applyWithWitness RelsWitness delta (EPlayerSpot attrs modes rels) =
        EPlayerSpot attrs modes (apply delta rels)

instance IncrementalWithWitness 'Table where
    applyWithWitness AttrsWitness delta (ETable attrs modes rels) =
        ETable (apply delta attrs) modes rels
    applyWithWitness ModesWitness delta (ETable attrs modes rels) =
        ETable attrs (apply delta modes) rels
    applyWithWitness RelsWitness delta (ETable attrs modes rels) =
        ETable attrs modes (apply delta rels)

instance IncrementalWithWitness 'TableShoe where
    applyWithWitness AttrsWitness delta (ETableShoe attrs modes rels) =
        ETableShoe (apply delta attrs) modes rels
    applyWithWitness ModesWitness delta (ETableShoe attrs modes rels) =
        ETableShoe attrs (apply delta modes) rels
    applyWithWitness RelsWitness delta (ETableShoe attrs modes rels) =
        ETableShoe attrs modes (apply delta rels)

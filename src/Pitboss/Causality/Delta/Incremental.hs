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

mkEntityRef :: Tick -> EntityIdFor k -> EntityRef k
mkEntityRef tick entityId = EntityRef (Uid (tick, entityId))

instance Incremental BoutAttrs where
    type Applicable BoutAttrs = Delta 'Bout (PartialUpdate 'Attrs)

    apply (DBoutSetOutcome new _) attrs = attrs{_bAttrsOutcome = new}

    describe (DBoutSetOutcome new old) _ = "Set bout outcome: " ++ show old ++ " → " ++ show new

instance Incremental BoutModes where
    type Applicable BoutModes = Delta 'Bout (PartialUpdate 'Modes)

    apply (DBoutSetFSM new _) modes = modes{_bModesFSM = new}

    describe (DBoutSetFSM new old) _ = "Set bout FSM: " ++ show old ++ " → " ++ show new

instance Incremental BoutRels where
    type Applicable BoutRels = Delta 'Bout (PartialUpdate 'Rels)

    apply (DBoutSetPlayerHand new _) rels = rels{_bRelsPlayerHand = new}
    apply (DBoutSetDealerHand new _) rels = rels{_bRelsDealerHand = new}
    apply (DBoutSetRound new _) rels = rels{_bRelsRound = new}
    apply (DBoutSetShoe new _) rels = rels{_bRelsShoe = new}
    apply (DBoutSetTable new _) rels = rels{_bRelsTable = new}

    describe (DBoutSetPlayerHand new old) _ = "Set bout player hand: " ++ show old ++ " → " ++ show new
    describe (DBoutSetDealerHand new old) _ = "Set bout dealer hand: " ++ show old ++ " → " ++ show new
    describe (DBoutSetRound new old) _ = "Set bout round: " ++ show old ++ " → " ++ show new
    describe (DBoutSetShoe new old) _ = "Set bout shoe: " ++ show old ++ " → " ++ show new
    describe (DBoutSetTable new old) _ = "Set bout table: " ++ show old ++ " → " ++ show new

instance IncrementalWithWitness 'Bout where
    applyWithWitness AttrsWitness delta (EBout attrs modes rels) =
        EBout (apply delta attrs) modes rels
    applyWithWitness ModesWitness delta (EBout attrs modes rels) =
        EBout attrs (apply delta modes) rels
    applyWithWitness RelsWitness delta (EBout attrs modes rels) =
        EBout attrs modes (apply delta rels)

instance Incremental ContestantAttrs where
    type Applicable ContestantAttrs = Delta 'Contestant (PartialUpdate 'Attrs)

    apply (DContestantSetActiveHandIx new _) attrs = attrs{_cAttrsActiveHandIx = new}

    describe (DContestantSetActiveHandIx new old) _ = "Set active hand index: " ++ show old ++ " → " ++ show new

instance Incremental ContestantModes where
    type Applicable ContestantModes = Delta 'Contestant (PartialUpdate 'Modes)

    apply (DContestantSetBoutFSM new _) modes = modes{_cModesBoutFSM = new}
    apply (DContestantSetRoundFSM new _) modes = modes{_cModesRoundFSM = new}

    describe (DContestantSetBoutFSM new old) _ = "Set contestant bout FSM: " ++ show old ++ " → " ++ show new
    describe (DContestantSetRoundFSM new old) _ = "Set contestant round FSM: " ++ show old ++ " → " ++ show new

instance Incremental ContestantRels where
    type Applicable ContestantRels = Delta 'Contestant (PartialUpdate 'Rels)

    apply (DContestantSetBoutEntry ix new _) rels =
        rels{_cRelsBouts = insertFiniteMap ix new (_cRelsBouts rels)}
    apply (DContestantInsertBoutEntry ix new) rels =
        rels{_cRelsBouts = insertFiniteMap ix new (_cRelsBouts rels)}
    apply (DContestantSetPlayer new _) rels = rels{_cRelsPlayer = new}
    apply (DContestantSetRound new _) rels = rels{_cRelsRound = new}
    apply (DContestantSetShoe new _) rels = rels{_cRelsShoe = new}

    describe (DContestantSetBoutEntry ix new old) _ = "Set contestant bout at " ++ show ix ++ ": " ++ show old ++ " → " ++ show new
    describe (DContestantInsertBoutEntry ix new) _ = "Insert contestant bout at " ++ show ix ++ ": " ++ show new
    describe (DContestantSetPlayer new old) _ = "Set contestant player: " ++ show old ++ " → " ++ show new
    describe (DContestantSetRound new old) _ = "Set contestant round: " ++ show old ++ " → " ++ show new
    describe (DContestantSetShoe new old) _ = "Set contestant shoe: " ++ show old ++ " → " ++ show new

instance IncrementalWithWitness 'Contestant where
    applyWithWitness AttrsWitness delta (EContestant attrs modes rels) =
        EContestant (apply delta attrs) modes rels
    applyWithWitness ModesWitness delta (EContestant attrs modes rels) =
        EContestant attrs (apply delta modes) rels
    applyWithWitness RelsWitness delta (EContestant attrs modes rels) =
        EContestant attrs modes (apply delta rels)

instance Incremental DealerAttrs where
    type Applicable DealerAttrs = Delta 'Dealer (PartialUpdate 'Attrs)

    apply (DDealerSetName new _) attrs = attrs{_dAttrsName = new}

    describe (DDealerSetName new old) _ = "Set dealer name: " ++ show old ++ " → " ++ show new

instance Incremental DealerModes where
    type Applicable DealerModes = Delta 'Dealer (PartialUpdate 'Modes)

    apply (DDealerSetTableFSM new _) modes = modes{_dModesDealerTable = new}
    apply (DDealerSetRoundFSM new _) modes = modes{_dModesRound = new}

    describe (DDealerSetTableFSM new old) _ = "Set dealer table FSM: " ++ show old ++ " → " ++ show new
    describe (DDealerSetRoundFSM new old) _ = "Set dealer round FSM: " ++ show old ++ " → " ++ show new

instance Incremental DealerRels where
    type Applicable DealerRels = Delta 'Dealer (PartialUpdate 'Rels)

    apply (DDealerSetActiveHand new _) rels = rels{_dRelsActiveHand = new}
    apply (DDealerSetActiveRound new _) rels = rels{_dRelsActiveRound = new}
    apply (DDealerSetActiveTable new _) rels = rels{_dRelsActiveTable = new}

    describe (DDealerSetActiveHand new old) _ = "Set dealer active hand: " ++ show old ++ " → " ++ show new
    describe (DDealerSetActiveRound new old) _ = "Set dealer active round: " ++ show old ++ " → " ++ show new
    describe (DDealerSetActiveTable new old) _ = "Set dealer active table: " ++ show old ++ " → " ++ show new

instance IncrementalWithWitness 'Dealer where
    applyWithWitness AttrsWitness delta (EDealer attrs modes rels) =
        EDealer (apply delta attrs) modes rels
    applyWithWitness ModesWitness delta (EDealer attrs modes rels) =
        EDealer attrs (apply delta modes) rels
    applyWithWitness RelsWitness delta (EDealer attrs modes rels) =
        EDealer attrs modes (apply delta rels)

instance Incremental HandAttrs where
    type Applicable HandAttrs = Delta 'Hand (PartialUpdate 'Attrs)

    apply (DHandSetHand new _) attrs = attrs{_hAttrsHand = new}

    describe (DHandSetHand new old) _ = "Set hand: " ++ show old ++ " → " ++ show new

instance Incremental HandModes where
    type Applicable HandModes = Delta 'Hand (PartialUpdate 'Modes)

    apply (DHandSetHandFSM new _) modes = modes{_hModesHandFSM = new}

    describe (DHandSetHandFSM new old) _ = "Set hand FSM: " ++ show old ++ " → " ++ show new

instance Incremental HandRels where
    type Applicable HandRels = Delta 'Hand (PartialUpdate 'Rels)

    apply (DHandSetOwner new _) rels = rels{_hRelsOwner = new}
    apply (DHandSetBout new _) rels = rels{_hRelsBout = new}
    apply (DHandSetShoe new _) rels = rels{_hRelsShoe = new}

    describe (DHandSetOwner new old) _ = "Set hand owner: " ++ show old ++ " → " ++ show new
    describe (DHandSetBout new old) _ = "Set hand bout: " ++ show old ++ " → " ++ show new
    describe (DHandSetShoe new old) _ = "Set hand shoe: " ++ show old ++ " → " ++ show new

instance IncrementalWithWitness 'Hand where
    applyWithWitness AttrsWitness delta (EHand attrs modes rels) =
        EHand (apply delta attrs) modes rels
    applyWithWitness ModesWitness delta (EHand attrs modes rels) =
        EHand attrs (apply delta modes) rels
    applyWithWitness RelsWitness delta (EHand attrs modes rels) =
        EHand attrs modes (apply delta rels)

instance Incremental PlayerAttrs where
    type Applicable PlayerAttrs = Delta 'Player (PartialUpdate 'Attrs)

    apply (DPlayerSetName new _) attrs = attrs{_pAttrsName = new}
    apply (DPlayerSetBankroll new _) attrs = attrs{_pAttrsBankroll = new}

    describe (DPlayerSetName new old) _ = "Set player name: " ++ show old ++ " → " ++ show new
    describe (DPlayerSetBankroll new old) _ = "Set player bankroll: " ++ show old ++ " → " ++ show new

instance Incremental PlayerModes where
    type Applicable PlayerModes = Delta 'Player (PartialUpdate 'Modes)

    apply (DPlayerSetPlayerFSM new _) modes = modes{_pModesPlayerFSM = new}

    describe (DPlayerSetPlayerFSM new old) _ = "Set player FSM: " ++ show old ++ " → " ++ show new

instance Incremental PlayerRels where
    type Applicable PlayerRels = Delta 'Player (PartialUpdate 'Rels)

    apply (DPlayerSetActiveRound new _) rels = rels{_pRelsActiveRound = new}
    apply (DPlayerSetActiveTable new _) rels = rels{_pRelsActiveTable = new}

    describe (DPlayerSetActiveRound new old) _ = "Set player active round: " ++ show old ++ " → " ++ show new
    describe (DPlayerSetActiveTable new old) _ = "Set player active table: " ++ show old ++ " → " ++ show new

instance IncrementalWithWitness 'Player where
    applyWithWitness AttrsWitness delta (EPlayer attrs modes rels) =
        EPlayer (apply delta attrs) modes rels
    applyWithWitness ModesWitness delta (EPlayer attrs modes rels) =
        EPlayer attrs (apply delta modes) rels
    applyWithWitness RelsWitness delta (EPlayer attrs modes rels) =
        EPlayer attrs modes (apply delta rels)

instance Incremental RoundAttrs where
    type Applicable RoundAttrs = Delta 'Round (PartialUpdate 'Attrs)

    apply (DRoundSetNumber new _) attrs = attrs{_rAttrsNumber = new}
    apply (DRoundSetActiveSpotIx new _) attrs = attrs{_rAttrsActiveSpotIx = new}

    describe (DRoundSetNumber new old) _ = "Set round number: " ++ show old ++ " → " ++ show new
    describe (DRoundSetActiveSpotIx new old) _ = "Set active spot index: " ++ show old ++ " → " ++ show new

instance Incremental RoundModes where
    type Applicable RoundModes = Delta 'Round (PartialUpdate 'Modes)

    apply _ modes = modes

    describe _ _ = "No change to round modes"

instance Incremental RoundRels where
    type Applicable RoundRels = Delta 'Round (PartialUpdate 'Rels)

    apply (DRoundSetDealerHand new _) rels = rels{_rRelsDealerHand = new}
    apply (DRoundSetContestantEntry ix new _) rels =
        rels{_rRelsContestants = insertFiniteMap ix new (_rRelsContestants rels)}
    apply (DRoundInsertContestantEntry ix new) rels =
        rels{_rRelsContestants = insertFiniteMap ix new (_rRelsContestants rels)}
    apply (DRoundSetShoe new _) rels = rels{_rRelsShoe = new}
    apply (DRoundSetTable new _) rels = rels{_rRelsTable = new}

    describe (DRoundSetDealerHand new old) _ = "Set round dealer hand: " ++ show old ++ " → " ++ show new
    describe (DRoundSetContestantEntry ix new old) _ = "Set round contestant at " ++ show ix ++ ": " ++ show old ++ " → " ++ show new
    describe (DRoundInsertContestantEntry ix new) _ = "Insert round contestant at " ++ show ix ++ ": " ++ show new
    describe (DRoundSetShoe new old) _ = "Set round shoe: " ++ show old ++ " → " ++ show new
    describe (DRoundSetTable new old) _ = "Set round table: " ++ show old ++ " → " ++ show new

instance IncrementalWithWitness 'Round where
    applyWithWitness AttrsWitness delta (ERound attrs modes rels) =
        ERound (apply delta attrs) modes rels
    applyWithWitness ModesWitness delta (ERound attrs modes rels) =
        ERound attrs (apply delta modes) rels
    applyWithWitness RelsWitness delta (ERound attrs modes rels) =
        ERound attrs modes (apply delta rels)

instance Incremental ShoeAttrs where
    type Applicable ShoeAttrs = Delta 'Shoe (PartialUpdate 'Attrs)

    apply (DShoeSetCards new _) attrs = attrs{_sAttrsCards = new}
    apply (DShoeSetCardStateMap new _) attrs = attrs{_sAttrsCardStates = new}
    apply (DShoeSetCardFate ix new _) attrs =
        attrs{_sAttrsCardStates = Map.insert ix new (_sAttrsCardStates attrs)}
    apply (DShoeInsertCardFate ix new) attrs =
        attrs{_sAttrsCardStates = Map.insert ix new (_sAttrsCardStates attrs)}

    describe (DShoeSetCards _ _) _ = "Set shoe cards"
    describe (DShoeSetCardStateMap _ _) _ = "Set card state map"
    describe (DShoeSetCardFate ix new old) _ = "Set card fate at index " ++ show ix ++ ": " ++ show old ++ " → " ++ show new
    describe (DShoeInsertCardFate ix new) _ = "Insert card fate at index " ++ show ix ++ ": " ++ show new

instance Incremental ShoeModes where
    type Applicable ShoeModes = Delta 'Shoe (PartialUpdate 'Modes)

    apply _ modes = modes

    describe _ _ = "No change to shoe modes"

instance Incremental ShoeRels where
    type Applicable ShoeRels = Delta 'Shoe (PartialUpdate 'Rels)

    apply (DShoeSetTable new _) rels = rels{_sRelsTable = new}

    describe (DShoeSetTable new old) _ = "Set shoe table: " ++ show old ++ " → " ++ show new

instance IncrementalWithWitness 'Shoe where
    applyWithWitness AttrsWitness delta (EShoe attrs modes rels) =
        EShoe (apply delta attrs) modes rels
    applyWithWitness ModesWitness delta (EShoe attrs modes rels) =
        EShoe attrs (apply delta modes) rels
    applyWithWitness RelsWitness delta (EShoe attrs modes rels) =
        EShoe attrs modes (apply delta rels)

instance Incremental TableAttrs where
    type Applicable TableAttrs = Delta 'Table (PartialUpdate 'Attrs)

    apply (DTableSetName new _) attrs = attrs{_tAttrsName = new}
    apply (DTableSetOffering new _) attrs = attrs{_tAttrsOffering = new}
    apply (DTableSetSeatEntry ix new _) attrs =
        attrs{_tAttrsSeats = insertFiniteMap ix new (_tAttrsSeats attrs)}
    apply (DTableInsertSeatEntry ix new) attrs =
        attrs{_tAttrsSeats = insertFiniteMap ix new (_tAttrsSeats attrs)}

    describe (DTableSetName new old) _ = "Set table name: " ++ show old ++ " → " ++ show new
    describe (DTableSetOffering new old) _ = "Set table offering: " ++ show old ++ " → " ++ show new
    describe (DTableSetSeatEntry ix new old) _ = "Set seat at " ++ show ix ++ ": " ++ show old ++ " → " ++ show new
    describe (DTableInsertSeatEntry ix new) _ = "Insert seat at " ++ show ix ++ ": " ++ show new

instance Incremental TableModes where
    type Applicable TableModes = Delta 'Table (PartialUpdate 'Modes)

    apply (DTableSetFSM new _) modes = modes{_tModesFSM = new}

    describe (DTableSetFSM new old) _ = "Set table FSM: " ++ show old ++ " → " ++ show new

instance Incremental TableRels where
    type Applicable TableRels = Delta 'Table (PartialUpdate 'Rels)

    apply (DTableSetActiveDealer new _) rels = rels{_tRelsActiveDealer = new}
    apply (DTableSetActiveRound new _) rels = rels{_tRelsActiveRound = new}

    describe (DTableSetActiveDealer new old) _ = "Set table active dealer: " ++ show old ++ " → " ++ show new
    describe (DTableSetActiveRound new old) _ = "Set table active round: " ++ show old ++ " → " ++ show new

instance IncrementalWithWitness 'Table where
    applyWithWitness AttrsWitness delta (ETable attrs modes rels) =
        ETable (apply delta attrs) modes rels
    applyWithWitness ModesWitness delta (ETable attrs modes rels) =
        ETable attrs (apply delta modes) rels
    applyWithWitness RelsWitness delta (ETable attrs modes rels) =
        ETable attrs modes (apply delta rels)

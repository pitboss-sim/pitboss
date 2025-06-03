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

class Incremental target where
    type Applicable target = applicable | applicable -> target

    apply :: Applicable target -> target -> target

    describe :: Applicable target -> target -> String

instance Incremental BoutAttrs where
    type Applicable BoutAttrs = Delta 'Bout (PartialUpdate 'Attrs)

    apply (DBoutSetPlayerHand new _) attrs = attrs{_bAttrsPlayerHand = new}
    apply (DBoutSetDealerHand new _) attrs = attrs{_bAttrsDealerHand = new}
    apply (DBoutSetActiveHandIx new _) attrs = attrs{_bAttrsActiveHandIx = new}
    apply (DBoutSetOutcome new _) attrs = attrs{_bAttrsOutcome = new}

    describe (DBoutSetPlayerHand new old) _ = "Set bout player hand: " ++ show old ++ " → " ++ show new
    describe (DBoutSetDealerHand new old) _ = "Set bout dealer hand: " ++ show old ++ " → " ++ show new
    describe (DBoutSetActiveHandIx new old) _ = "Set bout active hand index: " ++ show old ++ " → " ++ show new
    describe (DBoutSetOutcome new old) _ = "Set bout outcome: " ++ show old ++ " → " ++ show new

instance Incremental BoutModes where
    type Applicable BoutModes = Delta 'Bout (PartialUpdate 'Modes)

    apply (DBoutSetBoutFSM new _) modes = modes{_bModesBoutFSM = new}
    apply (DBoutSetPlayerHandFSM new _) modes = modes{_bModesPlayerHandFSM = new}
    apply (DBoutSetDealerHandFSM new _) modes = modes{_bModesDealerHandFSM = new}

    describe (DBoutSetBoutFSM new old) _ = "Set bout FSM: " ++ show old ++ " → " ++ show new
    describe (DBoutSetPlayerHandFSM new old) _ = "Set bout player hand FSM: " ++ show old ++ " → " ++ show new
    describe (DBoutSetDealerHandFSM new old) _ = "Set bout dealer hand FSM: " ++ show old ++ " → " ++ show new

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

    apply (DDealerSetActiveRound new _) rels = rels{_dRelsActiveRound = new}
    apply (DDealerSetActiveTable new _) rels = rels{_dRelsActiveTable = new}

    describe (DDealerSetActiveRound new old) _ = "Set dealer active round: " ++ show old ++ " → " ++ show new
    describe (DDealerSetActiveTable new old) _ = "Set dealer active table: " ++ show old ++ " → " ++ show new

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

instance Incremental BoutRels where
    type Applicable BoutRels = Delta 'Bout (PartialUpdate 'Rels)

    apply (DBoutSetPlayer new _) rels = rels{_bRelsPlayer = new}
    apply (DBoutSetDealer new _) rels = rels{_bRelsDealer = new}
    apply (DBoutSetRound new _) rels = rels{_bRelsRound = new}
    apply (DBoutSetTable new _) rels = rels{_bRelsTable = new}
    apply (DBoutSetPlayerBoutEntry ix new _) rels = rels{_bRelsPlayerBouts = insertFiniteMap ix new (_bRelsPlayerBouts rels)}

    describe (DBoutSetPlayer new old) _ = "Set bout player: " ++ show old ++ " → " ++ show new
    describe (DBoutSetDealer new old) _ = "Set bout dealer: " ++ show old ++ " → " ++ show new
    describe (DBoutSetRound new old) _ = "Set bout round: " ++ show old ++ " → " ++ show new
    describe (DBoutSetTable new old) _ = "Set bout table: " ++ show old ++ " → " ++ show new
    describe (DBoutSetPlayerBoutEntry ix new old) _ = "Set bout player bout at " ++ show ix ++ ": " ++ show old ++ " → " ++ show new

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

    apply (DRoundSetPlayerEntry ix new _) rels =
        rels{_rRelsBouts = insertFiniteMap ix new (_rRelsBouts rels)}
    apply (DRoundSetShoe new _) rels = rels{_rRelsShoe = new}
    apply (DRoundSetTable new _) rels = rels{_rRelsTable = new}

    describe (DRoundSetPlayerEntry ix new old) _ = "Set round player at " ++ show ix ++ ": " ++ show old ++ " → " ++ show new
    describe (DRoundSetShoe new old) _ = "Set round shoe: " ++ show old ++ " → " ++ show new
    describe (DRoundSetTable new old) _ = "Set round table: " ++ show old ++ " → " ++ show new

instance Incremental ShoeAttrs where
    type Applicable ShoeAttrs = Delta 'Shoe (PartialUpdate 'Attrs)

    apply (DShoeSetCards new _) attrs = attrs{_sAttrsCards = new}
    apply (DShoeSetCardStateMap new _) attrs = attrs{_sAttrsCardStates = new}
    apply (DShoeSetCardFate ix new _) attrs =
        attrs{_sAttrsCardStates = Map.insert ix new (_sAttrsCardStates attrs)}

    describe (DShoeSetCards _ _) _ = "Set shoe cards"
    describe (DShoeSetCardStateMap _ _) _ = "Set card state map"
    describe (DShoeSetCardFate ix new old) _ = "Set card fate at index " ++ show ix ++ ": " ++ show old ++ " → " ++ show new

instance Incremental ShoeModes where
    type Applicable ShoeModes = Delta 'Shoe (PartialUpdate 'Modes)

    apply _ modes = modes

    describe _ _ = "No change to shoe modes"

instance Incremental ShoeRels where
    type Applicable ShoeRels = Delta 'Shoe (PartialUpdate 'Rels)

    apply (DShoeSetTable new _) rels = rels{_sRelsTable = new}

    describe (DShoeSetTable new old) _ = "Set shoe table: " ++ show old ++ " → " ++ show new

instance Incremental TableAttrs where
    type Applicable TableAttrs = Delta 'Table (PartialUpdate 'Attrs)

    apply (DTableSetName new _) attrs = attrs{_tAttrsName = new}
    apply (DTableSetOffering new _) attrs = attrs{_tAttrsOffering = new}
    apply (DTableSetSeatEntry ix new _) attrs =
        attrs{_tAttrsSeats = insertFiniteMap ix new (_tAttrsSeats attrs)}

    describe (DTableSetName new old) _ = "Set table name: " ++ show old ++ " → " ++ show new
    describe (DTableSetOffering new old) _ = "Set table offering: " ++ show old ++ " → " ++ show new
    describe (DTableSetSeatEntry ix new old) _ = "Set seat at " ++ show ix ++ ": " ++ show old ++ " → " ++ show new

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

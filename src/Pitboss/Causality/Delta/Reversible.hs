{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Pitboss.Causality.Delta.Reversible where

import Pitboss.Causality.Delta.Types
import Pitboss.Causality.Types.Core

data InversionError
    = NotInvertible
    | MissingPriorContext String
    | CustomReason String
    deriving (Eq, Show)

class Reversible d where
    invert :: d -> Either InversionError d

instance Reversible (Delta 'Bout (PartialUpdate 'Attrs)) where
    invert (DBoutSetOutcome old new) = Right (DBoutSetOutcome new old)

instance Reversible (Delta 'Bout (PartialUpdate 'Modes)) where
    invert (DBoutSetFSM old new) = Right (DBoutSetFSM new old)

instance Reversible (Delta 'Bout (PartialUpdate 'Rels)) where
    invert (DBoutSetPlayerHand old new) = Right (DBoutSetPlayerHand new old)
    invert (DBoutSetDealerHand old new) = Right (DBoutSetDealerHand new old)
    invert (DBoutSetRound old new) = Right (DBoutSetRound new old)
    invert (DBoutSetShoe old new) = Right (DBoutSetShoe new old)
    invert (DBoutSetTable old new) = Right (DBoutSetTable new old)

instance Reversible (Delta 'Contestant (PartialUpdate 'Attrs)) where
    invert (DContestantSetActiveHandIx old new) = Right (DContestantSetActiveHandIx new old)

instance Reversible (Delta 'Contestant (PartialUpdate 'Modes)) where
    invert (DContestantSetBoutFSM old new) = Right (DContestantSetBoutFSM new old)
    invert (DContestantSetRoundFSM old new) = Right (DContestantSetRoundFSM new old)

instance Reversible (Delta 'Contestant (PartialUpdate 'Rels)) where
    invert (DContestantSetBoutEntry ix old new) = Right (DContestantSetBoutEntry ix new old)
    invert (DContestantInsertBoutEntry _ _) = Left (CustomReason "Cannot reverse append-only bout insertion")
    invert (DContestantSetPlayer old new) = Right (DContestantSetPlayer new old)
    invert (DContestantSetRound old new) = Right (DContestantSetRound new old)
    invert (DContestantSetShoe old new) = Right (DContestantSetShoe new old)

instance Reversible (Delta 'Dealer (PartialUpdate 'Attrs)) where
    invert (DDealerSetName old new) = Right (DDealerSetName new old)

instance Reversible (Delta 'Dealer (PartialUpdate 'Modes)) where
    invert (DDealerSetTableFSM old new) = Right (DDealerSetTableFSM new old)
    invert (DDealerSetRoundFSM old new) = Right (DDealerSetRoundFSM new old)

instance Reversible (Delta 'Dealer (PartialUpdate 'Rels)) where
    invert (DDealerSetActiveHand old new) = Right (DDealerSetActiveHand new old)
    invert (DDealerSetActiveRound old new) = Right (DDealerSetActiveRound new old)
    invert (DDealerSetActiveTable old new) = Right (DDealerSetActiveTable new old)

instance Reversible (Delta 'Hand (PartialUpdate 'Attrs)) where
    invert (DHandSetHand old new) = Right (DHandSetHand new old)

instance Reversible (Delta 'Hand (PartialUpdate 'Modes)) where
    invert (DHandSetHandFSM old new) = Right (DHandSetHandFSM new old)

instance Reversible (Delta 'Hand (PartialUpdate 'Rels)) where
    invert (DHandSetOwner old new) = Right (DHandSetOwner new old)
    invert (DHandSetBout old new) = Right (DHandSetBout new old)
    invert (DHandSetShoe old new) = Right (DHandSetShoe new old)

instance Reversible (Delta 'Player (PartialUpdate 'Attrs)) where
    invert (DPlayerSetName old new) = Right (DPlayerSetName new old)
    invert (DPlayerSetBankroll old new) = Right (DPlayerSetBankroll new old)

instance Reversible (Delta 'Player (PartialUpdate 'Modes)) where
    invert (DPlayerSetPlayerFSM old new) = Right (DPlayerSetPlayerFSM new old)

instance Reversible (Delta 'Player (PartialUpdate 'Rels)) where
    invert (DPlayerSetActiveRound old new) = Right (DPlayerSetActiveRound new old)
    invert (DPlayerSetActiveTable old new) = Right (DPlayerSetActiveTable new old)

instance Reversible (Delta 'Round (PartialUpdate 'Attrs)) where
    invert (DRoundSetNumber old new) = Right (DRoundSetNumber new old)
    invert (DRoundSetActiveSpotIx old new) = Right (DRoundSetActiveSpotIx new old)

instance Reversible (Delta 'Round (PartialUpdate 'Modes)) where
    invert _ = Left (CustomReason "Round modes have no reversible operations")

instance Reversible (Delta 'Round (PartialUpdate 'Rels)) where
    invert (DRoundSetDealerHand old new) = Right (DRoundSetDealerHand new old)
    invert (DRoundSetContestantEntry ix old new) = Right (DRoundSetContestantEntry ix new old)
    invert (DRoundInsertContestantEntry _ _) = Left (CustomReason "Cannot reverse append-only contestant insertion")
    invert (DRoundSetShoe old new) = Right (DRoundSetShoe new old)
    invert (DRoundSetTable old new) = Right (DRoundSetTable new old)

instance Reversible (Delta 'Shoe (PartialUpdate 'Attrs)) where
    invert (DShoeSetCards old new) = Right (DShoeSetCards new old)
    invert (DShoeSetCardStateMap old new) = Right (DShoeSetCardStateMap new old)
    invert (DShoeSetCardFate ix old new) = Right (DShoeSetCardFate ix new old)
    invert (DShoeInsertCardFate _ _) = Left (CustomReason "Cannot reverse append-only card fate insertion")

instance Reversible (Delta 'Shoe (PartialUpdate 'Modes)) where
    invert _ = Left (CustomReason "Shoe modes have no reversible operations")

instance Reversible (Delta 'Shoe (PartialUpdate 'Rels)) where
    invert (DShoeSetTable old new) = Right (DShoeSetTable new old)

instance Reversible (Delta 'Table (PartialUpdate 'Attrs)) where
    invert (DTableSetName old new) = Right (DTableSetName new old)
    invert (DTableSetOffering old new) = Right (DTableSetOffering new old)
    invert (DTableSetSeatEntry ix old new) = Right (DTableSetSeatEntry ix new old)
    invert (DTableInsertSeatEntry _ _) = Left (CustomReason "Cannot reverse append-only seat insertion")

instance Reversible (Delta 'Table (PartialUpdate 'Modes)) where
    invert (DTableSetFSM old new) = Right (DTableSetFSM new old)

instance Reversible (Delta 'Table (PartialUpdate 'Rels)) where
    invert (DTableSetActiveDealer old new) = Right (DTableSetActiveDealer new old)
    invert (DTableSetActiveRound old new) = Right (DTableSetActiveRound new old)

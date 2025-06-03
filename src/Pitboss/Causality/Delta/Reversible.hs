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

-- DIntent
instance Reversible (Delta 'Intent (PartialUpdate 'Attrs)) where
    invert (DIntentSetType old new) = Right (DIntentSetType new old)
    invert (DIntentSetKind old new) = Right (DIntentSetKind new old)
    invert (DIntentSetTimestamp old new) = Right (DIntentSetTimestamp new old)
    invert (DIntentSetDescription old new) = Right (DIntentSetDescription new old)

instance Reversible (Delta 'Intent (PartialUpdate 'Modes)) where
    invert _ = Left (CustomReason "Intent modes have no reversible operations")

instance Reversible (Delta 'Intent (PartialUpdate 'Rels)) where
    invert (DIntentSetOriginatingEntity old new) = Right (DIntentSetOriginatingEntity new old)
    invert (DIntentSetTargetBout old new) = Right (DIntentSetTargetBout new old)

-- DBout
instance Reversible (Delta 'Bout (PartialUpdate 'Attrs)) where
    invert (DBoutSetOutcome old new) = Right (DBoutSetOutcome new old)

instance Reversible (Delta 'Bout (PartialUpdate 'Modes)) where
    invert (DBoutSetFSM old new) = Right (DBoutSetFSM new old)

instance Reversible (Delta 'Bout (PartialUpdate 'Rels)) where
    invert (DBoutSetPlayerHand old new) = Right (DBoutSetPlayerHand new old)
    invert (DBoutSetDealerHand old new) = Right (DBoutSetDealerHand new old)
    invert (DBoutSetTableShoe old new) = Right (DBoutSetTableShoe new old)
    invert (DBoutSetTable old new) = Right (DBoutSetTable new old)
    invert (DBoutSetDealerRound old new) = Right (DBoutSetDealerRound new old)

-- DDealer
instance Reversible (Delta 'Dealer (PartialUpdate 'Attrs)) where
    invert (DDealerSetName old new) = Right (DDealerSetName new old)

instance Reversible (Delta 'Dealer (PartialUpdate 'Modes)) where
    invert (DDealerSetTableFSM old new) = Right (DDealerSetTableFSM new old)
    invert (DDealerSetRoundFSM old new) = Right (DDealerSetRoundFSM new old)
    invert (DDealerSetHandFSM old new) = Right (DDealerSetHandFSM new old)

instance Reversible (Delta 'Dealer (PartialUpdate 'Rels)) where
    invert (DDealerSetActiveTable old new) = Right (DDealerSetActiveTable new old)
    invert (DDealerSetActiveRound old new) = Right (DDealerSetActiveRound new old)
    invert (DDealerSetActiveHand old new) = Right (DDealerSetActiveHand new old)

-- DDealerHand
instance Reversible (Delta 'DealerHand (PartialUpdate 'Attrs)) where
    invert (DDealerHandSetHand old new) = Right (DDealerHandSetHand new old)

instance Reversible (Delta 'DealerHand (PartialUpdate 'Modes)) where
    invert (DDealerHandSetFSM old new) = Right (DDealerHandSetFSM new old)

instance Reversible (Delta 'DealerHand (PartialUpdate 'Rels)) where
    invert (DDealerHandSetRound old new) = Right (DDealerHandSetRound new old)
    invert (DDealerHandSetDealer old new) = Right (DDealerHandSetDealer new old)

-- DDealerRound
instance Reversible (Delta 'DealerRound (PartialUpdate 'Attrs)) where
    invert (DDealerRoundSetNumber old new) = Right (DDealerRoundSetNumber new old)

instance Reversible (Delta 'DealerRound (PartialUpdate 'Modes)) where
    invert _ = Left (CustomReason "DealerRound modes have no reversible operations")

instance Reversible (Delta 'DealerRound (PartialUpdate 'Rels)) where
    invert (DDealerRoundSetTableShoe old new) = Right (DDealerRoundSetTableShoe new old)

-- DPlayer
instance Reversible (Delta 'Player (PartialUpdate 'Attrs)) where
    invert (DPlayerSetName old new) = Right (DPlayerSetName new old)
    invert (DPlayerSetBankroll old new) = Right (DPlayerSetBankroll new old)

instance Reversible (Delta 'Player (PartialUpdate 'Modes)) where
    invert (DPlayerSetTable old new) = Right (DPlayerSetTable new old)
    invert (DPlayerSetSpot old new) = Right (DPlayerSetSpot new old)
    invert (DPlayerSetHand old new) = Right (DPlayerSetHand new old)

instance Reversible (Delta 'Player (PartialUpdate 'Rels)) where
    invert _ = Left (CustomReason "Player relations have no reversible operations")

-- DPlayerHand
instance Reversible (Delta 'PlayerHand (PartialUpdate 'Attrs)) where
    invert (DPlayerHandSetPlayerHandIx old new) = Right (DPlayerHandSetPlayerHandIx new old)
    invert (DPlayerHandSetSplitDepth old new) = Right (DPlayerHandSetSplitDepth new old)
    invert (DPlayerHandSetHand old new) = Right (DPlayerHandSetHand new old)

instance Reversible (Delta 'PlayerHand (PartialUpdate 'Modes)) where
    invert (DPlayerHandSetPlayerHandFSM old new) = Right (DPlayerHandSetPlayerHandFSM new old)

instance Reversible (Delta 'PlayerHand (PartialUpdate 'Rels)) where
    invert (DPlayerHandSetPlayerSpot old new) = Right (DPlayerHandSetPlayerSpot new old)

-- DPlayerSpot
instance Reversible (Delta 'PlayerSpot (PartialUpdate 'Attrs)) where
    invert (DPlayerSpotSetWager old new) = Right (DPlayerSpotSetWager new old)

instance Reversible (Delta 'PlayerSpot (PartialUpdate 'Modes)) where
    invert (DPlayerSpotSetFSM old new) = Right (DPlayerSpotSetFSM new old)

instance Reversible (Delta 'PlayerSpot (PartialUpdate 'Rels)) where
    invert (DPlayerSpotSetPlayer old new) = Right (DPlayerSpotSetPlayer new old)
    invert (DPlayerSpotSetRound old new) = Right (DPlayerSpotSetRound new old)
    invert (DPlayerSpotSetHandOccupancy old new) = Right (DPlayerSpotSetHandOccupancy new old)

-- DTable
instance Reversible (Delta 'Table (PartialUpdate 'Attrs)) where
    invert (DTableSetName old new) = Right (DTableSetName new old)
    invert (DTableSetOffering old new) = Right (DTableSetOffering new old)

instance Reversible (Delta 'Table (PartialUpdate 'Modes)) where
    invert _ = Left (CustomReason "Table modes have no reversible operations")

instance Reversible (Delta 'Table (PartialUpdate 'Rels)) where
    invert (DTableSetDealer old new) = Right (DTableSetDealer new old)

-- DTableShoe
instance Reversible (Delta 'TableShoe (PartialUpdate 'Attrs)) where
    invert (DTableShoeSetCardStateMap old new) = Right (DTableShoeSetCardStateMap new old)
    invert (DTableShoeSetCardFate ix old new) = Right (DTableShoeSetCardFate ix new old)

instance Reversible (Delta 'TableShoe (PartialUpdate 'Modes)) where
    invert _ = Left (CustomReason "TableShoe modes have no reversible operations")

instance Reversible (Delta 'TableShoe (PartialUpdate 'Rels)) where
    invert (DTableShoeSetTable old new) = Right (DTableShoeSetTable new old)

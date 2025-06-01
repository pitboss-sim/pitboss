{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Pitboss.State.Delta.Instances.Reversible where

import Pitboss.State.Delta.Types
import Pitboss.State.Types.Core

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
    invert (DIntentSetOriginatingEntity a b) = Right (DIntentSetOriginatingEntity b a)
    invert (DIntentSetTargetBout a b) = Right (DIntentSetTargetBout b a)

-- DEvent
instance Reversible (Delta 'Event (PartialUpdate 'Attrs)) where
    invert (DEventSetType old new) = Right (DEventSetType new old)
    invert (DEventSetDetails old new) = Right (DEventSetDetails new old)
    invert (DEventSetTimestamp old new) = Right (DEventSetTimestamp new old)
    invert (DEventSetDescription old new) = Right (DEventSetDescription new old)

instance Reversible (Delta 'Event (PartialUpdate 'Modes)) where
    invert _ = Left (CustomReason "Event modes have no reversible operations")

instance Reversible (Delta 'Event (PartialUpdate 'Rels)) where
    invert (DEventSetCausingIntent a b) = Right (DEventSetCausingIntent b a)

-- DBout
instance Reversible (Delta 'Bout (PartialUpdate 'Attrs)) where
    invert (DBoutSetOutcome old new) = Right (DBoutSetOutcome new old)

instance Reversible (Delta 'Bout (PartialUpdate 'Modes)) where
    invert (DBoutSetFSM old new) = Right (DBoutSetFSM new old)

instance Reversible (Delta 'Bout (PartialUpdate 'Rels)) where
    invert (DBoutSetPlayerHand a b) = Right (DBoutSetPlayerHand b a)
    invert (DBoutSetDealerHand a b) = Right (DBoutSetDealerHand b a)
    invert (DBoutSetTableShoe a b) = Right (DBoutSetTableShoe b a)
    invert (DBoutSetTable a b) = Right (DBoutSetTable b a)
    invert (DBoutSetDealerRound a b) = Right (DBoutSetDealerRound b a)

-- DDealer
instance Reversible (Delta 'Dealer (PartialUpdate 'Attrs)) where
    invert (DDealerSetName old new) = Right (DDealerSetName new old)

instance Reversible (Delta 'Dealer (PartialUpdate 'Modes)) where
    invert (DDealerSetTableFSM old new) = Right (DDealerSetTableFSM new old)
    invert (DDealerSetRoundFSM old new) = Right (DDealerSetRoundFSM new old)
    invert (DDealerSetHandFSM old new) = Right (DDealerSetHandFSM new old)

instance Reversible (Delta 'Dealer (PartialUpdate 'Rels)) where
    invert (DDealerSetActiveTable a b) = Right (DDealerSetActiveTable b a)
    invert (DDealerSetActiveRound a b) = Right (DDealerSetActiveRound b a)
    invert (DDealerSetActiveHand a b) = Right (DDealerSetActiveHand b a)

-- DDealerHand
instance Reversible (Delta 'DealerHand (PartialUpdate 'Attrs)) where
    invert (DDealerHandSetHand old new) = Right (DDealerHandSetHand new old)

instance Reversible (Delta 'DealerHand (PartialUpdate 'Modes)) where
    invert (DDealerHandSetFSM old new) = Right (DDealerHandSetFSM new old)

instance Reversible (Delta 'DealerHand (PartialUpdate 'Rels)) where
    invert (DDealerHandSetRound a b) = Right (DDealerHandSetRound b a)
    invert (DDealerHandSetDealer a b) = Right (DDealerHandSetDealer b a)

-- DDealerRound
instance Reversible (Delta 'DealerRound (PartialUpdate 'Attrs)) where
    invert (DDealerRoundSetNumber old new) = Right (DDealerRoundSetNumber new old)

instance Reversible (Delta 'DealerRound (PartialUpdate 'Modes)) where
    invert _ = Left (CustomReason "DealerRound modes have no reversible operations")

instance Reversible (Delta 'DealerRound (PartialUpdate 'Rels)) where
    invert (DDealerRoundSetTableShoe a b) = Right (DDealerRoundSetTableShoe b a)

-- DOffering
instance Reversible (Delta 'Offering (PartialUpdate 'Attrs)) where
    invert (DOfferingSetOffering old new) = Right (DOfferingSetOffering new old)

instance Reversible (Delta 'Offering (PartialUpdate 'Modes)) where
    invert DOfferingModes = Right DOfferingModes

instance Reversible (Delta 'Offering (PartialUpdate 'Rels)) where
    invert DOfferingRels = Right DOfferingRels

-- DPlayer
instance Reversible (Delta 'Player (PartialUpdate 'Attrs)) where
    invert (DPlayerSetName old new) = Right (DPlayerSetName new old)
    invert (DPlayerSetBankroll old new) = Right (DPlayerSetBankroll new old)

instance Reversible (Delta 'Player (PartialUpdate 'Modes)) where
    invert (DPlayerSetTable a b) = Right (DPlayerSetTable b a)
    invert (DPlayerSetSpot a b) = Right (DPlayerSetSpot b a)
    invert (DPlayerSetHand a b) = Right (DPlayerSetHand b a)

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
    invert (DPlayerHandSetPlayerSpot a b) = Right (DPlayerHandSetPlayerSpot b a)

-- DPlayerSpot
instance Reversible (Delta 'PlayerSpot (PartialUpdate 'Attrs)) where
    invert (DPlayerSpotSetWager old new) = Right (DPlayerSpotSetWager new old)

instance Reversible (Delta 'PlayerSpot (PartialUpdate 'Modes)) where
    invert (DPlayerSpotSetFSM old new) = Right (DPlayerSpotSetFSM new old)

instance Reversible (Delta 'PlayerSpot (PartialUpdate 'Rels)) where
    invert (DPlayerSpotSetPlayer a b) = Right (DPlayerSpotSetPlayer b a)
    invert (DPlayerSpotSetRound a b) = Right (DPlayerSpotSetRound b a)
    invert (DPlayerSpotSetHandOccupancy a b) = Right (DPlayerSpotSetHandOccupancy b a)

-- DTable
instance Reversible (Delta 'Table (PartialUpdate 'Attrs)) where
    invert (DTableSetName old new) = Right (DTableSetName new old)
    invert (DTableSetOffering old new) = Right (DTableSetOffering new old)

instance Reversible (Delta 'Table (PartialUpdate 'Modes)) where
    invert _ = Left (CustomReason "Table modes have no reversible operations")

instance Reversible (Delta 'Table (PartialUpdate 'Rels)) where
    invert (DTableSetDealer a b) = Right (DTableSetDealer b a)

-- DTableShoe
instance Reversible (Delta 'TableShoe (PartialUpdate 'Attrs)) where
    invert (DTableShoeSetCardStateMap old new) = Right (DTableShoeSetCardStateMap new old)
    invert (DTableShoeSetCardFate ix old new) = Right (DTableShoeSetCardFate ix new old)

instance Reversible (Delta 'TableShoe (PartialUpdate 'Modes)) where
    invert _ = Left (CustomReason "TableShoe modes have no reversible operations")

instance Reversible (Delta 'TableShoe (PartialUpdate 'Rels)) where
    invert (DTableShoeSetTable a b) = Right (DTableShoeSetTable b a)

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Pitboss.Trace.Entity.Capability.Reversible where

import Pitboss.Trace.Entity.Delta
import Pitboss.Trace.Entity.Entity

-- DDeale-- D| Error type for when a delta cannot be reversed

data InversionError
    = NotInvertible
    | MissingPriorContext String
    | CustomReason String
    deriving (Eq, Show)

class Reversible d where
    invert :: d -> Either InversionError d

-- DDealer
instance Reversible (Delta 'Dealer 'Whole) where
    invert (DDealer a b c) = DDealer <$> invert a <*> invert b <*> invert c

instance Reversible (Delta 'Dealer (Part 'Attrs)) where
    invert (DDealerSetName old new) = Right (DDealerSetName new old)

instance Reversible (Delta 'Dealer (Part 'Modes)) where
    invert (DDealerSetTableFSM old new) = Right (DDealerSetTableFSM new old)
    invert (DDealerSetRoundFSM old new) = Right (DDealerSetRoundFSM new old)
    invert (DDealerSetHandFSM old new) = Right (DDealerSetHandFSM new old)

instance Reversible (Delta 'Dealer (Part 'Rels)) where
    invert (DDealerSetActiveTable a b) = Right (DDealerSetActiveTable b a)
    invert (DDealerSetActiveRound a b) = Right (DDealerSetActiveRound b a)
    invert (DDealerSetActiveHand a b) = Right (DDealerSetActiveHand b a)

-- DDealerHand
instance Reversible (Delta 'DealerHand 'Whole) where
    invert (DDealerHand a b c) = DDealerHand <$> invert a <*> invert b <*> invert c

instance Reversible (Delta 'DealerHand (Part 'Attrs)) where
    invert (DDealerHandPushCard c cs) = Right (DDealerHandPopCard c cs)
    invert (DDealerHandPopCard c cs) = Right (DDealerHandPushCard c cs)
    invert (DDealerHandSetCards old new) = Right (DDealerHandSetCards new old)

instance Reversible (Delta 'DealerHand (Part 'Modes)) where
    invert (DDealerHandSetFSM old new) = Right (DDealerHandSetFSM new old)

instance Reversible (Delta 'DealerHand (Part 'Rels)) where
    invert (DDealerHandSetRound a b) = Right (DDealerHandSetRound b a)
    invert (DDealerHandSetDealer a b) = Right (DDealerHandSetDealer b a)

-- DDealerRound
instance Reversible (Delta 'DealerRound 'Whole) where
    invert (DDealerRound a b c) = DDealerRound <$> invert a <*> invert b <*> invert c

instance Reversible (Delta 'DealerRound (Part 'Attrs)) where
    invert (DDealerRoundSetNumber old new) = Right (DDealerRoundSetNumber new old)

instance Reversible (Delta 'DealerRound (Part 'Modes)) where
    invert = Right -- DEmpty

instance Reversible (Delta 'DealerRound (Part 'Rels)) where
    invert (DDealerRoundSetTableShoe a b) = Right (DDealerRoundSetTableShoe b a)

-- DOffering
instance Reversible (Delta 'Offering 'Whole) where
    invert (DOffering a b c) = DOffering <$> invert a <*> invert b <*> invert c

instance Reversible (Delta 'Offering (Part 'Attrs)) where
    invert (DOfferingSetOffering old new) = Right (DOfferingSetOffering new old)

instance Reversible (Delta 'Offering (Part 'Modes)) where
    invert = Right

instance Reversible (Delta 'Offering (Part 'Rels)) where
    invert = Right

-- DPlayer
instance Reversible (Delta 'Player 'Whole) where
    invert (DPlayer a b c) = DPlayer <$> invert a <*> invert b <*> invert c

instance Reversible (Delta 'Player (Part 'Attrs)) where
    invert (DPlayerSetName old new) = Right (DPlayerSetName new old)
    invert (DPlayerSetBankroll old new) = Right (DPlayerSetBankroll new old)

instance Reversible (Delta 'Player (Part 'Modes)) where
    invert = Right

instance Reversible (Delta 'Player (Part 'Rels)) where
    invert (DPlayerSetTable a b) = Right (DPlayerSetTable b a)
    invert (DPlayerSetSpot a b) = Right (DPlayerSetSpot b a)
    invert (DPlayerSetHand a b) = Right (DPlayerSetHand b a)

-- DPlayerHand
instance Reversible (Delta 'PlayerHand 'Whole) where
    invert (DPlayerHand a b c) = DPlayerHand <$> invert a <*> invert b <*> invert c

instance Reversible (Delta 'PlayerHand (Part 'Attrs)) where
    invert (DPlayerHandSetPlayerHandIx old new) = Right (DPlayerHandSetPlayerHandIx new old)
    invert (DPlayerHandSetSplitDepth old new) = Right (DPlayerHandSetSplitDepth new old)
    invert (DPlayerHandPushCard c cs) = Right (DPlayerHandPopCard c cs)
    invert (DPlayerHandPopCard c cs) = Right (DPlayerHandPushCard c cs)
    invert (DPlayerHandSetCards old new) = Right (DPlayerHandSetCards new old)

instance Reversible (Delta 'PlayerHand (Part 'Modes)) where
    invert (DPlayerHandSetPlayerHandFSM old new) = Right (DPlayerHandSetPlayerHandFSM new old)

instance Reversible (Delta 'PlayerHand (Part 'Rels)) where
    invert (DPlayerHandSetPlayerSpot a b) = Right (DPlayerHandSetPlayerSpot b a)

-- DPlayerSpot
instance Reversible (Delta 'PlayerSpot 'Whole) where
    invert (DPlayerSpotAttrs a b c) = DPlayerSpotAttrs <$> invert a <*> invert b <*> invert c

instance Reversible (Delta 'PlayerSpot (Part 'Attrs)) where
    invert (DPlayerSpotSetWager old new) = Right (DPlayerSpotSetWager new old)

instance Reversible (Delta 'PlayerSpot (Part 'Modes)) where
    invert (DPlayerSpotSetFSM old new) = Right (DPlayerSpotSetFSM new old)

instance Reversible (Delta 'PlayerSpot (Part 'Rels)) where
    invert (DPlayerSpotSetPlayer a b) = Right (DPlayerSpotSetPlayer b a)
    invert (DPlayerSpotSetRound a b) = Right (DPlayerSpotSetRound b a)
    invert (DPlayerSpotSetHandOccupancy a b) = Right (DPlayerSpotSetHandOccupancy b a)

-- DTable
instance Reversible (Delta 'Table 'Whole) where
    invert (DTableAttrs a b c) = DTableAttrs <$> invert a <*> invert b <*> invert c

instance Reversible (Delta 'Table (Part 'Attrs)) where
    invert (DTableSetName old new) = Right (DTableSetName new old)
    invert (DTableSetMinBet old new) = Right (DTableSetMinBet new old)
    invert (DTableSetOffering old new) = Right (DTableSetOffering new old)

instance Reversible (Delta 'Table (Part 'Modes)) where
    invert = Right

instance Reversible (Delta 'Table (Part 'Rels)) where
    invert (DTableSetDealer a b) = Right (DTableSetDealer b a)

-- DTableShoe
instance Reversible (Delta 'TableShoe 'Whole) where
    invert (DTableShoe a b c) = DTableShoe <$> invert a <*> invert b <*> invert c

instance Reversible (Delta 'TableShoe (Part 'Attrs)) where
    invert (DTableShoeSetCardStateMap old new) = Right (DTableShoeSetCardStateMap new old)
    invert (DTableShoeSetCardFate ix s) = Right (DTableShoeSetCardFate ix s) -- Dnon-invertible, just id

instance Reversible (Delta 'TableShoe (Part 'Modes)) where
    invert = Right

instance Reversible (Delta 'TableShoe (Part 'Rels)) where
    invert (DTableShoeSetTable a b) = Right (DTableShoeSetTable b a)

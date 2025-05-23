{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Pitboss.Trace.Entity.Capability.Reversible where

import Pitboss.Trace.Entity.Delta
import Pitboss.Trace.Entity.Types

class Reversible d where
    invert :: d -> Either InversionError d

data InversionError
    = NotInvertible
    | MissingPriorContext String
    | CustomReason String
    deriving (Eq, Show)

-- DDealer

instance Reversible (Delta 'Dealer) where
    invert = \case
        DDealerAttrs' d -> DDealerAttrs' <$> invert d
        DDealerModes' d -> DDealerModes' <$> invert d
        DDealerRels' d -> DDealerRels' <$> invert d

instance Reversible DDealerAttrs where
    invert = \case
        DDealerSetName old new -> Right (DDealerSetName new old)

instance Reversible DDealerModes where
    invert = \case
        DDealerSetTableFSM old new -> Right (DDealerSetTableFSM new old)
        DDealerSetRoundFSM old new -> Right (DDealerSetRoundFSM new old)
        DDealerSetHandFSM old new -> Right (DDealerSetHandFSM new old)

instance Reversible DDealerRels where
    invert = \case
        DDealerSetActiveRound old new -> Right (DDealerSetActiveRound new old)
        DDealerSetActiveTable old new -> Right (DDealerSetActiveTable new old)
        DDealerSetActiveHand old new -> Right (DDealerSetActiveHand new old)

-- DDealerHand

instance Reversible (Delta 'DealerHand) where
    invert = \case
        DDealerHandAttrs' d -> DDealerHandAttrs' <$> invert d
        DDealerHandModes' d -> DDealerHandModes' <$> invert d
        DDealerHandRels' d -> DDealerHandRels' <$> invert d

instance Reversible DDealerHandAttrs where
    invert = \case
        DDealerHandPushCard c _ -> Right (DDealerHandPopCard c [])
        DDealerHandPopCard c _ -> Right (DDealerHandPushCard c [])
        DDealerHandSetCards old new -> Right (DDealerHandSetCards new old)

instance Reversible DDealerHandModes where
    invert (DDealerHandSetFSM old new) = Right (DDealerHandSetFSM new old)

instance Reversible DDealerHandRels where
    invert = \case
        DDealerHandSetRound old new -> Right (DDealerHandSetRound new old)
        DDealerHandSetDealer old new -> Right (DDealerHandSetDealer new old)

-- DDealerRound

instance Reversible (Delta 'DealerRound) where
    invert = \case
        DDealerRoundAttrs' d -> DDealerRoundAttrs' <$> invert d
        DDealerRoundModes' d -> DDealerRoundModes' <$> invert d
        DDealerRoundRels' d -> DDealerRoundRels' <$> invert d

instance Reversible DDealerRoundAttrs where
    invert (DDealerRoundSetNumber new old) =
        Right (DDealerRoundSetNumber old new)

instance Reversible DDealerRoundModes where
    invert DDealerRoundModes = Right DDealerRoundModes

instance Reversible DDealerRoundRels where
    invert (DDealerRoundSetTableShoe new old) =
        Right (DDealerRoundSetTableShoe old new)

-- Offering

instance Reversible (Delta 'Offering) where
    invert = \case
        DOfferingAttrs' d -> DOfferingAttrs' <$> invert d
        DOfferingModes' d -> DOfferingModes' <$> invert d
        DOfferingRels' d -> DOfferingRels' <$> invert d

instance Reversible DOfferingAttrs where
    invert (DOfferingSetOffering new old) =
        Right (DOfferingSetOffering old new)

instance Reversible DOfferingModes where
    invert (DOfferingModes _) = Right (DOfferingModes undefined)

instance Reversible DOfferingRels where
    invert (DOfferingRels _) = Right (DOfferingRels undefined)

-- Player

instance Reversible (Delta 'Player) where
    invert = \case
        DPlayerAttrs' d -> DPlayerAttrs' <$> invert d
        DPlayerModes' d -> DPlayerModes' <$> invert d
        DPlayerRels' d -> DPlayerRels' <$> invert d

instance Reversible DPlayerAttrs where
    invert = \case
        DPlayerSetName new old -> Right (DPlayerSetName old new)
        DPlayerSetBankroll new old -> Right (DPlayerSetBankroll old new)

instance Reversible DPlayerModes where
    invert DPlayerModes = Right DPlayerModes

instance Reversible DPlayerRels where
    invert = Right

-- PlayerHand

instance Reversible (Delta 'PlayerHand) where
    invert = \case
        DPlayerHandAttrs' d -> DPlayerHandAttrs' <$> invert d
        DPlayerHandModes' d -> DPlayerHandModes' <$> invert d
        DPlayerHandRels' d -> DPlayerHandRels' <$> invert d

instance Reversible DPlayerHandAttrs where
    invert = \case
        DPlayerHandSetPlayerHandIx new old -> Right (DPlayerHandSetPlayerHandIx old new)
        DPlayerHandSetSplitDepth new old -> Right (DPlayerHandSetSplitDepth old new)
        DPlayerHandPushCard c _ -> Right (DPlayerHandPopCard c [])
        DPlayerHandPopCard c _ -> Right (DPlayerHandPushCard c [])
        DPlayerHandSetCards new old -> Right (DPlayerHandSetCards old new)

instance Reversible DPlayerHandModes where
    invert (DPlayerHandSetPlayerHandFSM new old) = Right (DPlayerHandSetPlayerHandFSM old new)

instance Reversible DPlayerHandRels where
    invert (DPlayerHandSetPlayerSpot new old) = Right (DPlayerHandSetPlayerSpot old new)

-- PlayerSpot

instance Reversible (Delta 'PlayerSpot) where
    invert = \case
        DPlayerSpotAttrs' d -> DPlayerSpotAttrs' <$> invert d
        DPlayerSpotModes' d -> DPlayerSpotModes' <$> invert d
        DPlayerSpotRels' d -> DPlayerSpotRels' <$> invert d

instance Reversible DPlayerSpotAttrs where
    invert (DPlayerSpotSetWager new old) = Right (DPlayerSpotSetWager old new)

instance Reversible DPlayerSpotModes where
    invert (DPlayerSpotSetFSM new old) = Right (DPlayerSpotSetFSM old new)

instance Reversible DPlayerSpotRels where
    invert = \case
        DPlayerSpotSetPlayer new old -> Right (DPlayerSpotSetPlayer old new)
        DPlayerSpotSetRound new old -> Right (DPlayerSpotSetRound old new)
        DPlayerSpotSetHandOccupancy new old -> Right (DPlayerSpotSetHandOccupancy old new)

-- Table

instance Reversible (Delta 'Table) where
    invert = \case
        DTableAttrs' d -> DTableAttrs' <$> invert d
        DTableModes' d -> DTableModes' <$> invert d
        DTableRels' d -> DTableRels' <$> invert d

instance Reversible DTableAttrs where
    invert = \case
        DTableSetName new old -> Right (DTableSetName old new)
        DTableSetMinBet new old -> Right (DTableSetMinBet old new)
        DTableSetOffering new old -> Right (DTableSetOffering old new)

instance Reversible DTableModes where
    invert (DTableModes x) = Right (DTableModes x)

instance Reversible DTableRels where
    invert = \case
        DTableSetDealer new old -> Right (DTableSetDealer old new)

-- TableShoe

instance Reversible (Delta 'TableShoe) where
    invert = \case
        DTableShoeAttrs' d -> DTableShoeAttrs' <$> invert d
        DTableShoeModes' d -> DTableShoeModes' <$> invert d
        DTableShoeRels' d -> DTableShoeRels' <$> invert d

instance Reversible DTableShoeAttrs where
    invert = \case
        DTableShoeSetCardStateMap _ _ ->
            Left $ MissingPriorContext "Cannot invert DTableShoeSetCardStateMap' without previous state."
        DTableShoeSetCardFate ix _ ->
            Left $ MissingPriorContext $ "Cannot invert DTableShoeSetCardFate' at index " ++ show ix

instance Reversible DTableShoeModes where
    invert (DTableShoeModes x) = Right (DTableShoeModes x)

instance Reversible DTableShoeRels where
    invert = \case
        DTableShoeSetTable new old -> Right (DTableShoeSetTable old new)

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Pitboss.Trace.Entity.Capability.Reversible where

import Data.Maybe (fromMaybe)
import Pitboss.Trace.Entity.Dealer.Delta qualified as D
import Pitboss.Trace.Entity.DealerHand.Delta qualified as DH
import Pitboss.Trace.Entity.DealerRound.Delta
import Pitboss.Trace.Entity.DealerRound.Delta qualified as DR
import Pitboss.Trace.Entity.Delta
import Pitboss.Trace.Entity.Offering.Delta
import Pitboss.Trace.Entity.Offering.Delta qualified as O
import Pitboss.Trace.Entity.Player.Delta
import Pitboss.Trace.Entity.Player.Delta qualified as P
import Pitboss.Trace.Entity.PlayerHand.Delta qualified as PH
import Pitboss.Trace.Entity.PlayerSpot.Delta
import Pitboss.Trace.Entity.PlayerSpot.Delta qualified as PS
import Pitboss.Trace.Entity.Table.Delta
import Pitboss.Trace.Entity.Table.Delta qualified as T
import Pitboss.Trace.Entity.Types

class Reversible d where
    invert :: d -> Either InversionError d

data InversionError
    = NotInvertible
    | MissingPriorContext String
    | CustomReason String
    deriving (Eq, Show)

instance Reversible (Delta 'OfferingEntity) where
    invert = \case
        OfferingEntityAttrsDelta d -> OfferingEntityAttrsDelta <$> invert d
        OfferingEntityModesDelta d -> OfferingEntityModesDelta <$> invert d
        OfferingEntityRelsDelta d -> OfferingEntityRelsDelta <$> invert d

instance Reversible OfferingEntityAttrsDelta where
    invert = \case
        ReplaceOffering old new -> Right (ReplaceOffering new old)

instance Reversible OfferingEntityModesDelta where
    invert O.NoopModes = Right O.NoopModes

instance Reversible OfferingEntityRelsDelta where
    invert = \case
        AddTable tid -> Right (RemoveTable tid)
        RemoveTable tid -> Right (AddTable tid)

instance Reversible (Delta 'TableEntity) where
    invert = \case
        TableEntityAttrsDelta d -> TableEntityAttrsDelta <$> invert d
        TableEntityModesDelta d -> TableEntityModesDelta <$> invert d
        TableEntityRelsDelta d -> TableEntityRelsDelta <$> invert d

instance Reversible TableEntityAttrsDelta where
    invert = \case
        SetTableName old new -> Right (SetTableName new old)
        SetMinBet old new -> Right (SetMinBet new old)
        SetOffering old new -> Right (SetOffering new old)
        StartRound _ new -> Right (EndRound new)
        EndRound old -> Right (StartRound (Just old) old)

instance Reversible TableEntityModesDelta where
    invert T.NoopModes = Right T.NoopModes

instance Reversible TableEntityRelsDelta where
    invert = \case
        AssignDealer prev new -> Right (AssignDealer (Just new) (fromMaybe undefined prev))
        UnassignDealer old -> Right (AssignDealer (Just old) old)

instance Reversible (Delta 'TableShoeEntity) where
    invert Noop = Right Noop

instance Reversible (Delta 'DealerEntity) where
    invert = \case
        DealerEntityAttrsDelta d -> DealerEntityAttrsDelta <$> invert d
        DealerEntityModesDelta d -> DealerEntityModesDelta <$> invert d
        DealerEntityRelsDelta d -> DealerEntityRelsDelta <$> invert d

instance Reversible D.DealerEntityAttrsDelta where
    invert = \case
        D.RenameDealer old new -> Right (D.RenameDealer new old)
        D.ReplaceAssignedTable old new -> Right (D.ReplaceAssignedTable new old)

instance Reversible D.DealerEntityModesDelta where
    invert = \case
        D.ReplaceTableFSM old new -> Right (D.ReplaceTableFSM new old)
        D.ReplaceRoundFSM old new -> Right (D.ReplaceRoundFSM new old)
        D.ReplaceHandFSM old new -> Right (D.ReplaceHandFSM new old)

instance Reversible D.DealerEntityRelsDelta where
    invert = \case
        D.UpdateRound old new -> Right (D.UpdateRound new old)
        D.UpdateHand old new -> Right (D.UpdateHand new old)

instance Reversible (Delta 'DealerRoundEntity) where
    invert = \case
        DealerRoundEntityAttrsDelta d -> DealerRoundEntityAttrsDelta <$> invert d
        DealerRoundEntityModesDelta d -> DealerRoundEntityModesDelta <$> invert d
        DealerRoundEntityRelsDelta d -> DealerRoundEntityRelsDelta <$> invert d

instance Reversible DealerRoundEntityAttrsDelta where
    invert = \case
        SetDealerRoundEntityNumber _ -> Left NotInvertible
        SetActive b -> Right (SetActive (not b))

instance Reversible DealerRoundEntityModesDelta where
    invert DR.NoopModes = Right DR.NoopModes

instance Reversible DealerRoundEntityRelsDelta where
    invert = \case
        DR.SetTableShoeUsed _ -> Left NotInvertible

instance Reversible (Delta 'DealerHandEntity) where
    invert = \case
        DealerHandEntityAttrsDelta d -> DealerHandEntityAttrsDelta <$> invert d
        DealerHandEntityModesDelta d -> DealerHandEntityModesDelta <$> invert d
        DealerHandEntityRelsDelta d -> DealerHandEntityRelsDelta <$> invert d

instance Reversible DH.DealerHandEntityAttrsDelta where
    invert = \case
        DH.AddCard c -> Right (DH.RemoveCard c)
        DH.RemoveCard c -> Right (DH.AddCard c)
        DH.ReplaceCards old new -> Right (DH.ReplaceCards new old)

instance Reversible DH.DealerHandEntityModesDelta where
    invert (DH.ReplaceFSM old new) = Right (DH.ReplaceFSM new old)

instance Reversible DH.DealerHandEntityRelsDelta where
    invert = \case
        DH.UpdatePlayerSpot old new -> Right (DH.UpdatePlayerSpot new old)
        DH.UpdateDealerRound old new -> Right (DH.UpdateDealerRound new old)
        DH.UpdateDealer old new -> Right (DH.UpdateDealer new old)

instance Reversible (Delta 'PlayerEntity) where
    invert = \case
        PlayerEntityAttrsDelta d -> PlayerEntityAttrsDelta <$> invert d
        PlayerEntityModesDelta d -> PlayerEntityModesDelta <$> invert d
        PlayerEntityRelsDelta d -> PlayerEntityRelsDelta <$> invert d

instance Reversible PlayerEntityAttrsDelta where
    invert = \case
        RenamePlayer old new -> Right (RenamePlayer new old)
        SetBankroll old new -> Right (SetBankroll new old)

instance Reversible PlayerEntityModesDelta where
    invert P.NoopModes = Right P.NoopModes

instance Reversible PlayerEntityRelsDelta where
    invert = \case
        UpdateCloneOf old new -> Right (UpdateCloneOf new old)
        UpdateSeatedAt old new -> Right (UpdateSeatedAt new old)

instance Reversible (Delta 'PlayerHandEntity) where
    invert = \case
        PlayerHandEntityAttrsDelta d -> PlayerHandEntityAttrsDelta <$> invert d
        PlayerHandEntityModesDelta d -> PlayerHandEntityModesDelta <$> invert d
        PlayerHandEntityRelsDelta d -> PlayerHandEntityRelsDelta <$> invert d

instance Reversible PH.PlayerHandEntityAttrsDelta where
    invert = \case
        PH.AddCard c -> Right (PH.RemoveCard c)
        PH.RemoveCard c -> Right (PH.AddCard c)
        PH.ReplaceCards old new -> Right (PH.ReplaceCards new old)
        PH.ReplacePlayerHandIndex old new -> Right (PH.ReplacePlayerHandIndex new old)
        PH.ReplaceSplitDepth old new -> Right (PH.ReplaceSplitDepth new old)

instance Reversible PH.PlayerHandEntityModesDelta where
    invert (PH.ReplaceFSM old new) = Right (PH.ReplaceFSM new old)

instance Reversible PH.PlayerHandEntityRelsDelta where
    invert = \case
        PH.UpdatePlayerSpot old new -> Right (PH.UpdatePlayerSpot new old)
        PH.UpdateDealerRound old new -> Right (PH.UpdateDealerRound new old)
        PH.UpdatePlayer old new -> Right (PH.UpdatePlayer new old)

instance Reversible (Delta 'PlayerSpotEntity) where
    invert = \case
        PlayerSpotEntityAttrsDelta d -> PlayerSpotEntityAttrsDelta <$> invert d
        PlayerSpotEntityModesDelta d -> PlayerSpotEntityModesDelta <$> invert d
        PlayerSpotEntityRelsDelta d -> PlayerSpotEntityRelsDelta <$> invert d

instance Reversible PlayerSpotEntityAttrsDelta where
    invert = \case
        ReplaceWager old new -> Right (ReplaceWager new old)

instance Reversible PlayerSpotEntityModesDelta where
    invert (PS.ReplaceFSM old new) = Right (PS.ReplaceFSM new old)

instance Reversible PlayerSpotEntityRelsDelta where
    invert = \case
        PS.UpdatePlayer old new -> Right (PS.UpdatePlayer new old)
        PS.UpdateRound old new -> Right (PS.UpdateRound new old)
        PS.UpdateHandOccupancy old new -> Right (PS.UpdateHandOccupancy new old)

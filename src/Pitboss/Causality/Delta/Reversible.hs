{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Pitboss.Causality.Delta.Reversible where

import Data.Aeson.Types
import GHC.Generics (Generic)
import Pitboss.Causality.Delta.Types
import Pitboss.Causality.Types.Core

data InversionError
    = NotInvertible
    | MissingPriorContext String
    | CustomReason String
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

class Reversible d where
    invert :: d -> Either InversionError d

instance Reversible (Delta 'Dealer (PartialUpdate 'Attrs)) where
    invert (DDealerSetName old new) = Right (DDealerSetName new old)

instance Reversible (Delta 'Dealer (PartialUpdate 'Modes)) where
    invert (DDealerSetTableFSM old new) = Right (DDealerSetTableFSM new old)
    invert (DDealerSetRoundFSM old new) = Right (DDealerSetRoundFSM new old)

instance Reversible (Delta 'Dealer (PartialUpdate 'Rels)) where
    invert (DDealerSetActiveRound old new) = Right (DDealerSetActiveRound new old)
    invert (DDealerSetActiveTable old new) = Right (DDealerSetActiveTable new old)

instance Reversible (Delta 'Player (PartialUpdate 'Attrs)) where
    invert (DPlayerSetName old new) = Right (DPlayerSetName new old)
    invert (DPlayerSetBankroll old new) = Right (DPlayerSetBankroll new old)

instance Reversible (Delta 'Player (PartialUpdate 'Modes)) where
    invert (DPlayerSetPlayerFSM old new) = Right (DPlayerSetPlayerFSM new old)

instance Reversible (Delta 'Player (PartialUpdate 'Rels)) where
    invert (DPlayerSetActiveRound old new) = Right (DPlayerSetActiveRound new old)
    invert (DPlayerSetActiveTable old new) = Right (DPlayerSetActiveTable new old)

instance Reversible (Delta 'Bout (PartialUpdate 'Attrs)) where
    invert (DBoutSetPlayerHand old new) = Right (DBoutSetPlayerHand new old)
    invert (DBoutSetDealerHand old new) = Right (DBoutSetDealerHand new old)
    invert (DBoutSetActiveHandIx old new) = Right (DBoutSetActiveHandIx new old)
    invert (DBoutSetOutcome old new) = Right (DBoutSetOutcome new old)

instance Reversible (Delta 'Bout (PartialUpdate 'Modes)) where
    invert (DBoutSetBoutFSM old new) = Right (DBoutSetBoutFSM new old)
    invert (DBoutSetPlayerHandFSM old new) = Right (DBoutSetPlayerHandFSM new old)
    invert (DBoutSetDealerHandFSM old new) = Right (DBoutSetDealerHandFSM new old)

instance Reversible (Delta 'Bout (PartialUpdate 'Rels)) where
    invert (DBoutSetPlayer old new) = Right (DBoutSetPlayer new old)
    invert (DBoutSetDealer old new) = Right (DBoutSetDealer new old)
    invert (DBoutSetRound old new) = Right (DBoutSetRound new old)
    invert (DBoutSetTable old new) = Right (DBoutSetTable new old)
    invert (DBoutSetPlayerBoutEntry ix old new) = Right (DBoutSetPlayerBoutEntry ix new old)

instance Reversible (Delta 'Round (PartialUpdate 'Attrs)) where
    invert (DRoundSetNumber old new) = Right (DRoundSetNumber new old)
    invert (DRoundSetActiveSpotIx old new) = Right (DRoundSetActiveSpotIx new old)

instance Reversible (Delta 'Round (PartialUpdate 'Modes)) where
    invert _ = Left (CustomReason "Round modes have no reversible operations")

instance Reversible (Delta 'Round (PartialUpdate 'Rels)) where
    invert (DRoundSetPlayerEntry ix old new) = Right (DRoundSetPlayerEntry ix new old)
    invert (DRoundSetShoe old new) = Right (DRoundSetShoe new old)
    invert (DRoundSetTable old new) = Right (DRoundSetTable new old)

instance Reversible (Delta 'Shoe (PartialUpdate 'Attrs)) where
    invert (DShoeSetCards old new) = Right (DShoeSetCards new old)
    invert (DShoeSetCardStateMap old new) = Right (DShoeSetCardStateMap new old)
    invert (DShoeSetCardFate ix old new) = Right (DShoeSetCardFate ix new old)

instance Reversible (Delta 'Shoe (PartialUpdate 'Modes)) where
    invert _ = Left (CustomReason "Shoe modes have no reversible operations")

instance Reversible (Delta 'Shoe (PartialUpdate 'Rels)) where
    invert (DShoeSetTable old new) = Right (DShoeSetTable new old)

instance Reversible (Delta 'Table (PartialUpdate 'Attrs)) where
    invert (DTableSetName old new) = Right (DTableSetName new old)
    invert (DTableSetOffering old new) = Right (DTableSetOffering new old)
    invert (DTableSetSeatEntry ix old new) = Right (DTableSetSeatEntry ix new old)

instance Reversible (Delta 'Table (PartialUpdate 'Modes)) where
    invert (DTableSetFSM old new) = Right (DTableSetFSM new old)

instance Reversible (Delta 'Table (PartialUpdate 'Rels)) where
    invert (DTableSetActiveDealer old new) = Right (DTableSetActiveDealer new old)
    invert (DTableSetActiveRound old new) = Right (DTableSetActiveRound new old)

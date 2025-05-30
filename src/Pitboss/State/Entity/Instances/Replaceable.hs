{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Pitboss.State.Entity.Instances.Replaceable (
    ReplaceableAttrs (..),
    ReplaceableModes (..),
    ReplaceableRels (..),
) where

import Pitboss.State.Entity.Types
import Pitboss.State.Types.Core

class ReplaceableAttrs (k :: EntityKind) where
    replaceAttrs :: EntityState k -> EntityState k -> EntityState k

class ReplaceableModes (k :: EntityKind) where
    replaceModes :: EntityState k -> EntityState k -> EntityState k

class ReplaceableRels (k :: EntityKind) where
    replaceRels :: EntityState k -> EntityState k -> EntityState k

replaceBoutPart :: EntityStatePart -> EntityState 'Bout -> EntityState 'Bout -> EntityState 'Bout
replaceBoutPart Attrs (EBout newAttrs _ _) (EBout _ oldModes oldRels) = EBout newAttrs oldModes oldRels
replaceBoutPart Modes (EBout _ newModes _) (EBout oldAttrs _ oldRels) = EBout oldAttrs newModes oldRels
replaceBoutPart Rels  (EBout _ _ newRels) (EBout oldAttrs oldModes _) = EBout oldAttrs oldModes newRels

replaceDealerPart :: EntityStatePart -> EntityState 'Dealer -> EntityState 'Dealer -> EntityState 'Dealer
replaceDealerPart Attrs (EDealer newAttrs _ _) (EDealer _ oldModes oldRels) = EDealer newAttrs oldModes oldRels
replaceDealerPart Modes (EDealer _ newModes _) (EDealer oldAttrs _ oldRels) = EDealer oldAttrs newModes oldRels
replaceDealerPart Rels  (EDealer _ _ newRels) (EDealer oldAttrs oldModes _) = EDealer oldAttrs oldModes newRels

replaceDealerHandPart :: EntityStatePart -> EntityState 'DealerHand -> EntityState 'DealerHand -> EntityState 'DealerHand
replaceDealerHandPart Attrs (EDealerHand newAttrs _ _) (EDealerHand _ oldModes oldRels) = EDealerHand newAttrs oldModes oldRels
replaceDealerHandPart Modes (EDealerHand _ newModes _) (EDealerHand oldAttrs _ oldRels) = EDealerHand oldAttrs newModes oldRels
replaceDealerHandPart Rels  (EDealerHand _ _ newRels) (EDealerHand oldAttrs oldModes _) = EDealerHand oldAttrs oldModes newRels

replaceDealerRoundPart :: EntityStatePart -> EntityState 'DealerRound -> EntityState 'DealerRound -> EntityState 'DealerRound
replaceDealerRoundPart Attrs (EDealerRound newAttrs _ _) (EDealerRound _ oldModes oldRels) = EDealerRound newAttrs oldModes oldRels
replaceDealerRoundPart Modes (EDealerRound _ newModes _) (EDealerRound oldAttrs _ oldRels) = EDealerRound oldAttrs newModes oldRels
replaceDealerRoundPart Rels  (EDealerRound _ _ newRels) (EDealerRound oldAttrs oldModes _) = EDealerRound oldAttrs oldModes newRels

replaceOfferingPart :: EntityStatePart -> EntityState 'Offering -> EntityState 'Offering -> EntityState 'Offering
replaceOfferingPart Attrs (EOffering newAttrs _ _) (EOffering _ oldModes oldRels) = EOffering newAttrs oldModes oldRels
replaceOfferingPart Modes (EOffering _ newModes _) (EOffering oldAttrs _ oldRels) = EOffering oldAttrs newModes oldRels
replaceOfferingPart Rels  (EOffering _ _ newRels) (EOffering oldAttrs oldModes _) = EOffering oldAttrs oldModes newRels

replacePlayerPart :: EntityStatePart -> EntityState 'Player -> EntityState 'Player -> EntityState 'Player
replacePlayerPart Attrs (EPlayer newAttrs _ _) (EPlayer _ oldModes oldRels) = EPlayer newAttrs oldModes oldRels
replacePlayerPart Modes (EPlayer _ newModes _) (EPlayer oldAttrs _ oldRels) = EPlayer oldAttrs newModes oldRels
replacePlayerPart Rels  (EPlayer _ _ newRels) (EPlayer oldAttrs oldModes _) = EPlayer oldAttrs oldModes newRels

replacePlayerHandPart :: EntityStatePart -> EntityState 'PlayerHand -> EntityState 'PlayerHand -> EntityState 'PlayerHand
replacePlayerHandPart Attrs (EPlayerHand newAttrs _ _) (EPlayerHand _ oldModes oldRels) = EPlayerHand newAttrs oldModes oldRels
replacePlayerHandPart Modes (EPlayerHand _ newModes _) (EPlayerHand oldAttrs _ oldRels) = EPlayerHand oldAttrs newModes oldRels
replacePlayerHandPart Rels  (EPlayerHand _ _ newRels) (EPlayerHand oldAttrs oldModes _) = EPlayerHand oldAttrs oldModes newRels

replacePlayerSpotPart :: EntityStatePart -> EntityState 'PlayerSpot -> EntityState 'PlayerSpot -> EntityState 'PlayerSpot
replacePlayerSpotPart Attrs (EPlayerSpot newAttrs _ _) (EPlayerSpot _ oldModes oldRels) = EPlayerSpot newAttrs oldModes oldRels
replacePlayerSpotPart Modes (EPlayerSpot _ newModes _) (EPlayerSpot oldAttrs _ oldRels) = EPlayerSpot oldAttrs newModes oldRels
replacePlayerSpotPart Rels  (EPlayerSpot _ _ newRels) (EPlayerSpot oldAttrs oldModes _) = EPlayerSpot oldAttrs oldModes newRels

replaceTablePart :: EntityStatePart -> EntityState 'Table -> EntityState 'Table -> EntityState 'Table
replaceTablePart Attrs (ETable newAttrs _ _) (ETable _ oldModes oldRels) = ETable newAttrs oldModes oldRels
replaceTablePart Modes (ETable _ newModes _) (ETable oldAttrs _ oldRels) = ETable oldAttrs newModes oldRels
replaceTablePart Rels  (ETable _ _ newRels) (ETable oldAttrs oldModes _) = ETable oldAttrs oldModes newRels

replaceTableShoePart :: EntityStatePart -> EntityState 'TableShoe -> EntityState 'TableShoe -> EntityState 'TableShoe
replaceTableShoePart Attrs (ETableShoe newAttrs _ _) (ETableShoe _ oldModes oldRels) = ETableShoe newAttrs oldModes oldRels
replaceTableShoePart Modes (ETableShoe _ newModes _) (ETableShoe oldAttrs _ oldRels) = ETableShoe oldAttrs newModes oldRels
replaceTableShoePart Rels  (ETableShoe _ _ newRels) (ETableShoe oldAttrs oldModes _) = ETableShoe oldAttrs oldModes newRels

instance ReplaceableAttrs 'Bout where
    replaceAttrs = replaceBoutPart Attrs

instance ReplaceableModes 'Bout where
    replaceModes = replaceBoutPart Modes

instance ReplaceableRels 'Bout where
    replaceRels = replaceBoutPart Rels

instance ReplaceableAttrs 'Dealer where
    replaceAttrs = replaceDealerPart Attrs

instance ReplaceableModes 'Dealer where
    replaceModes = replaceDealerPart Modes

instance ReplaceableRels 'Dealer where
    replaceRels = replaceDealerPart Rels

instance ReplaceableAttrs 'DealerHand where
    replaceAttrs = replaceDealerHandPart Attrs

instance ReplaceableModes 'DealerHand where
    replaceModes = replaceDealerHandPart Modes

instance ReplaceableRels 'DealerHand where
    replaceRels = replaceDealerHandPart Rels

instance ReplaceableAttrs 'DealerRound where
    replaceAttrs = replaceDealerRoundPart Attrs

instance ReplaceableModes 'DealerRound where
    replaceModes = replaceDealerRoundPart Modes

instance ReplaceableRels 'DealerRound where
    replaceRels = replaceDealerRoundPart Rels

instance ReplaceableAttrs 'Offering where
    replaceAttrs = replaceOfferingPart Attrs

instance ReplaceableModes 'Offering where
    replaceModes = replaceOfferingPart Modes

instance ReplaceableRels 'Offering where
    replaceRels = replaceOfferingPart Rels

instance ReplaceableAttrs 'Player where
    replaceAttrs = replacePlayerPart Attrs

instance ReplaceableModes 'Player where
    replaceModes = replacePlayerPart Modes

instance ReplaceableRels 'Player where
    replaceRels = replacePlayerPart Rels

instance ReplaceableAttrs 'PlayerHand where
    replaceAttrs = replacePlayerHandPart Attrs

instance ReplaceableModes 'PlayerHand where
    replaceModes = replacePlayerHandPart Modes

instance ReplaceableRels 'PlayerHand where
    replaceRels = replacePlayerHandPart Rels

instance ReplaceableAttrs 'PlayerSpot where
    replaceAttrs = replacePlayerSpotPart Attrs

instance ReplaceableModes 'PlayerSpot where
    replaceModes = replacePlayerSpotPart Modes

instance ReplaceableRels 'PlayerSpot where
    replaceRels = replacePlayerSpotPart Rels

instance ReplaceableAttrs 'Table where
    replaceAttrs = replaceTablePart Attrs

instance ReplaceableModes 'Table where
    replaceModes = replaceTablePart Modes

instance ReplaceableRels 'Table where
    replaceRels = replaceTablePart Rels

instance ReplaceableAttrs 'TableShoe where
    replaceAttrs = replaceTableShoePart Attrs

instance ReplaceableModes 'TableShoe where
    replaceModes = replaceTableShoePart Modes

instance ReplaceableRels 'TableShoe where
    replaceRels = replaceTableShoePart Rels

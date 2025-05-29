{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE DataKinds #-}
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

-- EIntent
instance ReplaceableAttrs 'Intent where
    replaceAttrs (EIntent newAttrs _ _) (EIntent _ mo r) = EIntent newAttrs mo r

instance ReplaceableModes 'Intent where
    replaceModes (EIntent _ newModes _) (EIntent a _ r) = EIntent a newModes r

instance ReplaceableRels 'Intent where
    replaceRels (EIntent _ _ newRels) (EIntent a mo _) = EIntent a mo newRels

-- EEvent
instance ReplaceableAttrs 'Event where
    replaceAttrs (EEvent newAttrs _ _) (EEvent _ mo r) = EEvent newAttrs mo r

instance ReplaceableModes 'Event where
    replaceModes (EEvent _ newModes _) (EEvent a _ r) = EEvent a newModes r

instance ReplaceableRels 'Event where
    replaceRels (EEvent _ _ newRels) (EEvent a mo _) = EEvent a mo newRels

-- EBout
instance ReplaceableAttrs 'Bout where
    replaceAttrs (EBout newAttrs _ _) (EBout _ mo r) = EBout newAttrs mo r

instance ReplaceableModes 'Bout where
    replaceModes (EBout _ newModes _) (EBout a _ r) = EBout a newModes r

instance ReplaceableRels 'Bout where
    replaceRels (EBout _ _ newRels) (EBout a mo _) = EBout a mo newRels

-- EDealer
instance ReplaceableAttrs 'Dealer where
    replaceAttrs (EDealer newAttrs _ _) (EDealer _ mo r) = EDealer newAttrs mo r

instance ReplaceableModes 'Dealer where
    replaceModes (EDealer _ newModes _) (EDealer a _ r) = EDealer a newModes r

instance ReplaceableRels 'Dealer where
    replaceRels (EDealer _ _ newRels) (EDealer a mo _) = EDealer a mo newRels

-- EDealerHand
instance ReplaceableAttrs 'DealerHand where
    replaceAttrs (EDealerHand newAttrs _ _) (EDealerHand _ mo r) = EDealerHand newAttrs mo r

instance ReplaceableModes 'DealerHand where
    replaceModes (EDealerHand _ newModes _) (EDealerHand a _ r) = EDealerHand a newModes r

instance ReplaceableRels 'DealerHand where
    replaceRels (EDealerHand _ _ newRels) (EDealerHand a mo _) = EDealerHand a mo newRels

-- EDealerRound
instance ReplaceableAttrs 'DealerRound where
    replaceAttrs (EDealerRound newAttrs _ _) (EDealerRound _ mo r) = EDealerRound newAttrs mo r

instance ReplaceableModes 'DealerRound where
    replaceModes (EDealerRound _ newModes _) (EDealerRound a _ r) = EDealerRound a newModes r

instance ReplaceableRels 'DealerRound where
    replaceRels (EDealerRound _ _ newRels) (EDealerRound a mo _) = EDealerRound a mo newRels

-- EOffering
instance ReplaceableAttrs 'Offering where
    replaceAttrs (EOffering newAttrs _ _) (EOffering _ mo r) = EOffering newAttrs mo r

instance ReplaceableModes 'Offering where
    replaceModes (EOffering _ newModes _) (EOffering a _ r) = EOffering a newModes r

instance ReplaceableRels 'Offering where
    replaceRels (EOffering _ _ newRels) (EOffering a mo _) = EOffering a mo newRels

-- EPlayer
instance ReplaceableAttrs 'Player where
    replaceAttrs (EPlayer newAttrs _ _) (EPlayer _ mo r) = EPlayer newAttrs mo r

instance ReplaceableModes 'Player where
    replaceModes (EPlayer _ newModes _) (EPlayer a _ r) = EPlayer a newModes r

instance ReplaceableRels 'Player where
    replaceRels (EPlayer _ _ newRels) (EPlayer a mo _) = EPlayer a mo newRels

-- EPlayerHand
instance ReplaceableAttrs 'PlayerHand where
    replaceAttrs (EPlayerHand newAttrs _ _) (EPlayerHand _ mo r) = EPlayerHand newAttrs mo r

instance ReplaceableModes 'PlayerHand where
    replaceModes (EPlayerHand _ newModes _) (EPlayerHand a _ r) = EPlayerHand a newModes r

instance ReplaceableRels 'PlayerHand where
    replaceRels (EPlayerHand _ _ newRels) (EPlayerHand a mo _) = EPlayerHand a mo newRels

-- EPlayerSpot
instance ReplaceableAttrs 'PlayerSpot where
    replaceAttrs (EPlayerSpot newAttrs _ _) (EPlayerSpot _ mo r) = EPlayerSpot newAttrs mo r

instance ReplaceableModes 'PlayerSpot where
    replaceModes (EPlayerSpot _ newModes _) (EPlayerSpot a _ r) = EPlayerSpot a newModes r

instance ReplaceableRels 'PlayerSpot where
    replaceRels (EPlayerSpot _ _ newRels) (EPlayerSpot a mo _) = EPlayerSpot a mo newRels

-- ETable
instance ReplaceableAttrs 'Table where
    replaceAttrs (ETable newAttrs _ _) (ETable _ mo r) = ETable newAttrs mo r

instance ReplaceableModes 'Table where
    replaceModes (ETable _ newModes _) (ETable a _ r) = ETable a newModes r

instance ReplaceableRels 'Table where
    replaceRels (ETable _ _ newRels) (ETable a mo _) = ETable a mo newRels

-- ETableShoe
instance ReplaceableAttrs 'TableShoe where
    replaceAttrs (ETableShoe newAttrs _ _) (ETableShoe _ mo r) = ETableShoe newAttrs mo r

instance ReplaceableModes 'TableShoe where
    replaceModes (ETableShoe _ newModes _) (ETableShoe a _ r) = ETableShoe a newModes r

instance ReplaceableRels 'TableShoe where
    replaceRels (ETableShoe _ _ newRels) (ETableShoe a mo _) = ETableShoe a mo newRels

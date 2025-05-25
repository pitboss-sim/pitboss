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

class ReplaceableAttrs (k :: EntityKind) where
    replaceAttrs :: EntityState k ('Part 'Attrs) -> EntityState k 'Whole -> EntityState k 'Whole

class ReplaceableModes (k :: EntityKind) where
    replaceModes :: EntityState k ('Part 'Modes) -> EntityState k 'Whole -> EntityState k 'Whole

class ReplaceableRels (k :: EntityKind) where
    replaceRels :: EntityState k ('Part 'Rels) -> EntityState k 'Whole -> EntityState k 'Whole

-- Dealer
instance ReplaceableAttrs 'Dealer where
    replaceAttrs newAttrs (EDealer _ mo r) = EDealer newAttrs mo r
instance ReplaceableModes 'Dealer where
    replaceModes newModes (EDealer a _ r) = EDealer a newModes r
instance ReplaceableRels 'Dealer where
    replaceRels newRels (EDealer a mo _) = EDealer a mo newRels

-- DealerHand
instance ReplaceableAttrs 'DealerHand where
    replaceAttrs newAttrs (EDealerHand _ mo r) = EDealerHand newAttrs mo r
instance ReplaceableModes 'DealerHand where
    replaceModes newModes (EDealerHand a _ r) = EDealerHand a newModes r
instance ReplaceableRels 'DealerHand where
    replaceRels newRels (EDealerHand a mo _) = EDealerHand a mo newRels

-- DealerRound
instance ReplaceableAttrs 'DealerRound where
    replaceAttrs newAttrs (EDealerRound _ mo r) = EDealerRound newAttrs mo r
instance ReplaceableModes 'DealerRound where
    replaceModes newModes (EDealerRound a _ r) = EDealerRound a newModes r
instance ReplaceableRels 'DealerRound where
    replaceRels newRels (EDealerRound a mo _) = EDealerRound a mo newRels

-- Offering
instance ReplaceableAttrs 'Offering where
    replaceAttrs newAttrs (EOffering _ mo r) = EOffering newAttrs mo r
instance ReplaceableModes 'Offering where
    replaceModes newModes (EOffering a _ r) = EOffering a newModes r
instance ReplaceableRels 'Offering where
    replaceRels newRels (EOffering a mo _) = EOffering a mo newRels

-- Player
instance ReplaceableAttrs 'Player where
    replaceAttrs newAttrs (EPlayer _ mo r) = EPlayer newAttrs mo r
instance ReplaceableModes 'Player where
    replaceModes newModes (EPlayer a _ r) = EPlayer a newModes r
instance ReplaceableRels 'Player where
    replaceRels newRels (EPlayer a mo _) = EPlayer a mo newRels

-- PlayerHand
instance ReplaceableAttrs 'PlayerHand where
    replaceAttrs newAttrs (EPlayerHand _ mo r) = EPlayerHand newAttrs mo r
instance ReplaceableModes 'PlayerHand where
    replaceModes newModes (EPlayerHand a _ r) = EPlayerHand a newModes r
instance ReplaceableRels 'PlayerHand where
    replaceRels newRels (EPlayerHand a mo _) = EPlayerHand a mo newRels

-- PlayerSpot
instance ReplaceableAttrs 'PlayerSpot where
    replaceAttrs newAttrs (EPlayerSpot _ mo r) = EPlayerSpot newAttrs mo r
instance ReplaceableModes 'PlayerSpot where
    replaceModes newModes (EPlayerSpot a _ r) = EPlayerSpot a newModes r
instance ReplaceableRels 'PlayerSpot where
    replaceRels newRels (EPlayerSpot a mo _) = EPlayerSpot a mo newRels

-- Table
instance ReplaceableAttrs 'Table where
    replaceAttrs newAttrs (ETable _ mo r) = ETable newAttrs mo r
instance ReplaceableModes 'Table where
    replaceModes newModes (ETable a _ r) = ETable a newModes r
instance ReplaceableRels 'Table where
    replaceRels newRels (ETable a mo _) = ETable a mo newRels

-- TableShoe
instance ReplaceableAttrs 'TableShoe where
    replaceAttrs newAttrs (ETableShoe _ mo r) = ETableShoe newAttrs mo r
instance ReplaceableModes 'TableShoe where
    replaceModes newModes (ETableShoe a _ r) = ETableShoe a newModes r
instance ReplaceableRels 'TableShoe where
    replaceRels newRels (ETableShoe a mo _) = ETableShoe a mo newRels

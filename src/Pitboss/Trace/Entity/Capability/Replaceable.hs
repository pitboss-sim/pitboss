{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Pitboss.Trace.Entity.Capability.Replaceable where

import Pitboss.Trace.Entity.Entity

class ReplaceableAttrs (k :: EntityKind) where
    replaceAttrs :: EntityState k ('Part 'Attrs) -> EntityState k 'Whole -> EntityState k 'Whole

class ReplaceableModes (k :: EntityKind) where
    replaceModes :: EntityState k ('Part 'Modes) -> EntityState k 'Whole -> EntityState k 'Whole

class ReplaceableRels (k :: EntityKind) where
    replaceRels :: EntityState k ('Part 'Rels) -> EntityState k 'Whole -> EntityState k 'Whole

-- Dealer
instance ReplaceableAttrs 'Dealer where
    replaceAttrs newAttrs (EDealer m _ mo r) = EDealer m newAttrs mo r
instance ReplaceableModes 'Dealer where
    replaceModes newModes (EDealer m a _ r) = EDealer m a newModes r
instance ReplaceableRels 'Dealer where
    replaceRels newRels (EDealer m a mo _) = EDealer m a mo newRels

-- DealerHand
instance ReplaceableAttrs 'DealerHand where
    replaceAttrs newAttrs (EDealerHand m _ mo r) = EDealerHand m newAttrs mo r
instance ReplaceableModes 'DealerHand where
    replaceModes newModes (EDealerHand m a _ r) = EDealerHand m a newModes r
instance ReplaceableRels 'DealerHand where
    replaceRels newRels (EDealerHand m a mo _) = EDealerHand m a mo newRels

-- DealerRound
instance ReplaceableAttrs 'DealerRound where
    replaceAttrs newAttrs (EDealerRound m _ mo r) = EDealerRound m newAttrs mo r
instance ReplaceableModes 'DealerRound where
    replaceModes newModes (EDealerRound m a _ r) = EDealerRound m a newModes r
instance ReplaceableRels 'DealerRound where
    replaceRels newRels (EDealerRound m a mo _) = EDealerRound m a mo newRels

-- Offering
instance ReplaceableAttrs 'Offering where
    replaceAttrs newAttrs (EOffering m _ mo r) = EOffering m newAttrs mo r
instance ReplaceableModes 'Offering where
    replaceModes newModes (EOffering m a _ r) = EOffering m a newModes r
instance ReplaceableRels 'Offering where
    replaceRels newRels (EOffering m a mo _) = EOffering m a mo newRels

-- Player
instance ReplaceableAttrs 'Player where
    replaceAttrs newAttrs (EPlayer m _ mo r) = EPlayer m newAttrs mo r
instance ReplaceableModes 'Player where
    replaceModes newModes (EPlayer m a _ r) = EPlayer m a newModes r
instance ReplaceableRels 'Player where
    replaceRels newRels (EPlayer m a mo _) = EPlayer m a mo newRels

-- PlayerHand
instance ReplaceableAttrs 'PlayerHand where
    replaceAttrs newAttrs (EPlayerHand m _ mo r) = EPlayerHand m newAttrs mo r
instance ReplaceableModes 'PlayerHand where
    replaceModes newModes (EPlayerHand m a _ r) = EPlayerHand m a newModes r
instance ReplaceableRels 'PlayerHand where
    replaceRels newRels (EPlayerHand m a mo _) = EPlayerHand m a mo newRels

-- PlayerSpot
instance ReplaceableAttrs 'PlayerSpot where
    replaceAttrs newAttrs (EPlayerSpot m _ mo r) = EPlayerSpot m newAttrs mo r
instance ReplaceableModes 'PlayerSpot where
    replaceModes newModes (EPlayerSpot m a _ r) = EPlayerSpot m a newModes r
instance ReplaceableRels 'PlayerSpot where
    replaceRels newRels (EPlayerSpot m a mo _) = EPlayerSpot m a mo newRels

-- Table
instance ReplaceableAttrs 'Table where
    replaceAttrs newAttrs (ETable m _ mo r) = ETable m newAttrs mo r
instance ReplaceableModes 'Table where
    replaceModes newModes (ETable m a _ r) = ETable m a newModes r
instance ReplaceableRels 'Table where
    replaceRels newRels (ETable m a mo _) = ETable m a mo newRels

-- TableShoe
instance ReplaceableAttrs 'TableShoe where
    replaceAttrs newAttrs (ETableShoe m _ mo r) = ETableShoe m newAttrs mo r
instance ReplaceableModes 'TableShoe where
    replaceModes newModes (ETableShoe m a _ r) = ETableShoe m a newModes r
instance ReplaceableRels 'TableShoe where
    replaceRels newRels (ETableShoe m a mo _) = ETableShoe m a mo newRels

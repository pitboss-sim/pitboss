{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Pitboss.Trace.Entity.Capability.Replaceable where

import Pitboss.Trace.Entity.Entity

class ReplaceableAttrs (k :: EntityKind) where
    replaceAttrs :: EntityState k 'Whole -> EntityState k ('Part 'Attrs) -> EntityState k 'Whole

class ReplaceableModes (k :: EntityKind) where
    replaceModes :: EntityState k 'Whole -> EntityState k ('Part 'Modes) -> EntityState k 'Whole

class ReplaceableRels (k :: EntityKind) where
    replaceRels :: EntityState k 'Whole -> EntityState k ('Part 'Rels) -> EntityState k 'Whole

-- Dealer
instance ReplaceableAttrs 'Dealer where
    replaceAttrs (EDealer m _ mo r) a = EDealer m a mo r
instance ReplaceableModes 'Dealer where
    replaceModes (EDealer m a _ r) mo = EDealer m a mo r
instance ReplaceableRels 'Dealer where
    replaceRels (EDealer m a mo _) r = EDealer m a mo r

-- DealerHand

instance ReplaceableAttrs 'DealerHand where
    replaceAttrs (EDealerHand m _ mo r) a = EDealerHand m a mo r
instance ReplaceableModes 'DealerHand where
    replaceModes (EDealerHand m a _ r) mo = EDealerHand m a mo r
instance ReplaceableRels 'DealerHand where
    replaceRels (EDealerHand m a mo _) r = EDealerHand m a mo r

-- DealerRound

instance ReplaceableAttrs 'DealerRound where
    replaceAttrs (EDealerRound m _ mo r) a = EDealerRound m a mo r
instance ReplaceableModes 'DealerRound where
    replaceModes (EDealerRound m a _ r) mo = EDealerRound m a mo r
instance ReplaceableRels 'DealerRound where
    replaceRels (EDealerRound m a mo _) r = EDealerRound m a mo r

-- Offering
instance ReplaceableAttrs 'Offering where
    replaceAttrs (EOffering m _ mo r) a = EOffering m a mo r
instance ReplaceableModes 'Offering where
    replaceModes (EOffering m a _ r) mo = EOffering m a mo r
instance ReplaceableRels 'Offering where
    replaceRels (EOffering m a mo _) r = EOffering m a mo r

-- Player
instance ReplaceableAttrs 'Player where
    replaceAttrs (EPlayer m _ mo r) a = EPlayer m a mo r
instance ReplaceableModes 'Player where
    replaceModes (EPlayer m a _ r) mo = EPlayer m a mo r
instance ReplaceableRels 'Player where
    replaceRels (EPlayer m a mo _) r = EPlayer m a mo r

-- PlayerHand
instance ReplaceableAttrs 'PlayerHand where
    replaceAttrs (EPlayerHand m _ mo r) a = EPlayerHand m a mo r
instance ReplaceableModes 'PlayerHand where
    replaceModes (EPlayerHand m a _ r) mo = EPlayerHand m a mo r
instance ReplaceableRels 'PlayerHand where
    replaceRels (EPlayerHand m a mo _) r = EPlayerHand m a mo r

-- PlayerSpot
instance ReplaceableAttrs 'PlayerSpot where
    replaceAttrs (EPlayerSpot m _ mo r) a = EPlayerSpot m a mo r
instance ReplaceableModes 'PlayerSpot where
    replaceModes (EPlayerSpot m a _ r) mo = EPlayerSpot m a mo r
instance ReplaceableRels 'PlayerSpot where
    replaceRels (EPlayerSpot m a mo _) r = EPlayerSpot m a mo r

-- Table
instance ReplaceableAttrs 'Table where
    replaceAttrs (ETable m _ mo r) a = ETable m a mo r
instance ReplaceableModes 'Table where
    replaceModes (ETable m a _ r) mo = ETable m a mo r
instance ReplaceableRels 'Table where
    replaceRels (ETable m a mo _) r = ETable m a mo r

-- TableShoe
instance ReplaceableAttrs 'TableShoe where
    replaceAttrs (ETableShoe m _ mo r) a = ETableShoe m a mo r
instance ReplaceableModes 'TableShoe where
    replaceModes (ETableShoe m a _ r) mo = ETableShoe m a mo r
instance ReplaceableRels 'TableShoe where
    replaceRels (ETableShoe m a mo _) r = ETableShoe m a mo r

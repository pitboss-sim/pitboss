{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.Capability.Decomposable (
    Decomposable (..),
) where

import Pitboss.Trace.Entity.Entity

class Decomposable (k :: EntityKind) (s :: EntityStateSelector) where
    type Meta k s
    type Attrs k s
    type Modes k s
    type Rels k s

    getMeta :: EntityState k s -> Meta k s
    getAttrs :: EntityState k s -> Attrs k s
    getModes :: EntityState k s -> Modes k s
    getRels :: EntityState k s -> Rels k s

-- Dealer
instance Decomposable 'Dealer 'Whole where
    type Meta 'Dealer 'Whole = EntityState 'Dealer (Part 'Meta)
    type Attrs 'Dealer 'Whole = EntityState 'Dealer (Part 'Attrs)
    type Modes 'Dealer 'Whole = EntityState 'Dealer (Part 'Modes)
    type Rels 'Dealer 'Whole = EntityState 'Dealer (Part 'Rels)

    getMeta (EDealer m _ _ _) = m
    getAttrs (EDealer _ a _ _) = a
    getModes (EDealer _ _ m _) = m
    getRels (EDealer _ _ _ r) = r

-- DealerHand
instance Decomposable 'DealerHand 'Whole where
    type Meta 'DealerHand 'Whole = EntityState 'DealerHand (Part 'Meta)
    type Attrs 'DealerHand 'Whole = EntityState 'DealerHand (Part 'Attrs)
    type Modes 'DealerHand 'Whole = EntityState 'DealerHand (Part 'Modes)
    type Rels 'DealerHand 'Whole = EntityState 'DealerHand (Part 'Rels)

    getMeta (EDealerHand m _ _ _) = m
    getAttrs (EDealerHand _ a _ _) = a
    getModes (EDealerHand _ _ m _) = m
    getRels (EDealerHand _ _ _ r) = r

-- DealerRound
instance Decomposable 'DealerRound 'Whole where
    type Meta 'DealerRound 'Whole = EntityState 'DealerRound (Part 'Meta)
    type Attrs 'DealerRound 'Whole = EntityState 'DealerRound (Part 'Attrs)
    type Modes 'DealerRound 'Whole = EntityState 'DealerRound (Part 'Modes)
    type Rels 'DealerRound 'Whole = EntityState 'DealerRound (Part 'Rels)

    getMeta (EDealerRound m _ _ _) = m
    getAttrs (EDealerRound _ a _ _) = a
    getModes (EDealerRound _ _ m _) = m
    getRels (EDealerRound _ _ _ r) = r

-- Offering
instance Decomposable 'Offering 'Whole where
    type Meta 'Offering 'Whole = EntityState 'Offering (Part 'Meta)
    type Attrs 'Offering 'Whole = EntityState 'Offering (Part 'Attrs)
    type Modes 'Offering 'Whole = EntityState 'Offering (Part 'Modes)
    type Rels 'Offering 'Whole = EntityState 'Offering (Part 'Rels)

    getMeta (EOffering m _ _ _) = m
    getAttrs (EOffering _ a _ _) = a
    getModes (EOffering _ _ m _) = m
    getRels (EOffering _ _ _ r) = r

-- Player
instance Decomposable 'Player 'Whole where
    type Meta 'Player 'Whole = EntityState 'Player (Part 'Meta)
    type Attrs 'Player 'Whole = EntityState 'Player (Part 'Attrs)
    type Modes 'Player 'Whole = EntityState 'Player (Part 'Modes)
    type Rels 'Player 'Whole = EntityState 'Player (Part 'Rels)

    getMeta (EPlayer m _ _ _) = m
    getAttrs (EPlayer _ a _ _) = a
    getModes (EPlayer _ _ mo _) = mo
    getRels (EPlayer _ _ _ r) = r

-- PlayerHand
instance Decomposable 'PlayerHand 'Whole where
    type Meta 'PlayerHand 'Whole = EntityState 'PlayerHand (Part 'Meta)
    type Attrs 'PlayerHand 'Whole = EntityState 'PlayerHand (Part 'Attrs)
    type Modes 'PlayerHand 'Whole = EntityState 'PlayerHand (Part 'Modes)
    type Rels 'PlayerHand 'Whole = EntityState 'PlayerHand (Part 'Rels)

    getMeta (EPlayerHand m _ _ _) = m
    getAttrs (EPlayerHand _ a _ _) = a
    getModes (EPlayerHand _ _ m _) = m
    getRels (EPlayerHand _ _ _ r) = r

-- PlayerSpot
instance Decomposable 'PlayerSpot 'Whole where
    type Meta 'PlayerSpot 'Whole = EntityState 'PlayerSpot (Part 'Meta)
    type Attrs 'PlayerSpot 'Whole = EntityState 'PlayerSpot (Part 'Attrs)
    type Modes 'PlayerSpot 'Whole = EntityState 'PlayerSpot (Part 'Modes)
    type Rels 'PlayerSpot 'Whole = EntityState 'PlayerSpot (Part 'Rels)

    getMeta (EPlayerSpot m _ _ _) = m
    getAttrs (EPlayerSpot _ a _ _) = a
    getModes (EPlayerSpot _ _ m _) = m
    getRels (EPlayerSpot _ _ _ r) = r

-- Table
instance Decomposable 'Table 'Whole where
    type Meta 'Table 'Whole = EntityState 'Table (Part 'Meta)
    type Attrs 'Table 'Whole = EntityState 'Table (Part 'Attrs)
    type Modes 'Table 'Whole = EntityState 'Table (Part 'Modes)
    type Rels 'Table 'Whole = EntityState 'Table (Part 'Rels)

    getMeta (ETable m _ _ _) = m
    getAttrs (ETable _ a _ _) = a
    getModes (ETable _ _ m _) = m
    getRels (ETable _ _ _ r) = r

-- TableShoe
instance Decomposable 'TableShoe 'Whole where
    type Meta 'TableShoe 'Whole = EntityState 'TableShoe (Part 'Meta)
    type Attrs 'TableShoe 'Whole = EntityState 'TableShoe (Part 'Attrs)
    type Modes 'TableShoe 'Whole = EntityState 'TableShoe (Part 'Modes)
    type Rels 'TableShoe 'Whole = EntityState 'TableShoe (Part 'Rels)

    getMeta (ETableShoe m _ _ _) = m
    getAttrs (ETableShoe _ a _ _) = a
    getModes (ETableShoe _ _ m _) = m
    getRels (ETableShoe _ _ _ r) = r

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.State.Entity.Instances.Decomposable (
    Decomposable (..),
) where

import Pitboss.State.Entity.Types

class Decomposable (k :: EntityKind) (s :: EntityStateSelector) where
    type Attrs k s
    type Modes k s
    type Rels k s

    getAttrs :: EntityState k s -> Attrs k s
    getModes :: EntityState k s -> Modes k s
    getRels :: EntityState k s -> Rels k s

-- Dealer
instance Decomposable 'Dealer 'Whole where
    type Attrs 'Dealer 'Whole = EntityState 'Dealer (Part 'Attrs)
    type Modes 'Dealer 'Whole = EntityState 'Dealer (Part 'Modes)
    type Rels 'Dealer 'Whole = EntityState 'Dealer (Part 'Rels)

    getAttrs (EDealer a _ _) = a
    getModes (EDealer _ m _) = m
    getRels (EDealer _ _ r) = r

-- DealerHand
instance Decomposable 'DealerHand 'Whole where
    type Attrs 'DealerHand 'Whole = EntityState 'DealerHand (Part 'Attrs)
    type Modes 'DealerHand 'Whole = EntityState 'DealerHand (Part 'Modes)
    type Rels 'DealerHand 'Whole = EntityState 'DealerHand (Part 'Rels)

    getAttrs (EDealerHand a _ _) = a
    getModes (EDealerHand _ m _) = m
    getRels (EDealerHand _ _ r) = r

-- DealerRound
instance Decomposable 'DealerRound 'Whole where
    type Attrs 'DealerRound 'Whole = EntityState 'DealerRound (Part 'Attrs)
    type Modes 'DealerRound 'Whole = EntityState 'DealerRound (Part 'Modes)
    type Rels 'DealerRound 'Whole = EntityState 'DealerRound (Part 'Rels)

    getAttrs (EDealerRound a _ _) = a
    getModes (EDealerRound _ m _) = m
    getRels (EDealerRound _ _ r) = r

-- Offering
instance Decomposable 'Offering 'Whole where
    type Attrs 'Offering 'Whole = EntityState 'Offering (Part 'Attrs)
    type Modes 'Offering 'Whole = EntityState 'Offering (Part 'Modes)
    type Rels 'Offering 'Whole = EntityState 'Offering (Part 'Rels)

    getAttrs (EOffering a _ _) = a
    getModes (EOffering _ m _) = m
    getRels (EOffering _ _ r) = r

-- Player
instance Decomposable 'Player 'Whole where
    type Attrs 'Player 'Whole = EntityState 'Player (Part 'Attrs)
    type Modes 'Player 'Whole = EntityState 'Player (Part 'Modes)
    type Rels 'Player 'Whole = EntityState 'Player (Part 'Rels)

    getAttrs (EPlayer a _ _) = a
    getModes (EPlayer _ mo _) = mo
    getRels (EPlayer _ _ r) = r

-- PlayerHand
instance Decomposable 'PlayerHand 'Whole where
    type Attrs 'PlayerHand 'Whole = EntityState 'PlayerHand (Part 'Attrs)
    type Modes 'PlayerHand 'Whole = EntityState 'PlayerHand (Part 'Modes)
    type Rels 'PlayerHand 'Whole = EntityState 'PlayerHand (Part 'Rels)

    getAttrs (EPlayerHand a _ _) = a
    getModes (EPlayerHand _ m _) = m
    getRels (EPlayerHand _ _ r) = r

-- PlayerSpot
instance Decomposable 'PlayerSpot 'Whole where
    type Attrs 'PlayerSpot 'Whole = EntityState 'PlayerSpot (Part 'Attrs)
    type Modes 'PlayerSpot 'Whole = EntityState 'PlayerSpot (Part 'Modes)
    type Rels 'PlayerSpot 'Whole = EntityState 'PlayerSpot (Part 'Rels)

    getAttrs (EPlayerSpot a _ _) = a
    getModes (EPlayerSpot _ m _) = m
    getRels (EPlayerSpot _ _ r) = r

-- Table
instance Decomposable 'Table 'Whole where
    type Attrs 'Table 'Whole = EntityState 'Table (Part 'Attrs)
    type Modes 'Table 'Whole = EntityState 'Table (Part 'Modes)
    type Rels 'Table 'Whole = EntityState 'Table (Part 'Rels)

    getAttrs (ETable a _ _) = a
    getModes (ETable _ m _) = m
    getRels (ETable _ _ r) = r

-- TableShoe
instance Decomposable 'TableShoe 'Whole where
    type Attrs 'TableShoe 'Whole = EntityState 'TableShoe (Part 'Attrs)
    type Modes 'TableShoe 'Whole = EntityState 'TableShoe (Part 'Modes)
    type Rels 'TableShoe 'Whole = EntityState 'TableShoe (Part 'Rels)

    getAttrs (ETableShoe a _ _) = a
    getModes (ETableShoe _ m _) = m
    getRels (ETableShoe _ _ r) = r

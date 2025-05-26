{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.State.Entity.Instances.Decomposable (
    Decomposable (..),
) where

import Pitboss.State.Entity.Types

class Decomposable (k :: EntityKind) (s :: DeltaSemantics) where
    type Attrs k s
    type Modes k s
    type Rels k s

    getAttrs :: EntityState k s -> Attrs k s
    getModes :: EntityState k s -> Modes k s
    getRels :: EntityState k s -> Rels k s

-- Dealer
instance Decomposable 'Dealer 'TransactionBoundary where
    type Attrs 'Dealer 'TransactionBoundary = EntityState 'Dealer (PartialUpdate 'Attrs)
    type Modes 'Dealer 'TransactionBoundary = EntityState 'Dealer (PartialUpdate 'Modes)
    type Rels 'Dealer 'TransactionBoundary = EntityState 'Dealer (PartialUpdate 'Rels)

    getAttrs (EDealer a _ _) = a
    getModes (EDealer _ m _) = m
    getRels (EDealer _ _ r) = r

-- DealerHand
instance Decomposable 'DealerHand 'TransactionBoundary where
    type Attrs 'DealerHand 'TransactionBoundary = EntityState 'DealerHand (PartialUpdate 'Attrs)
    type Modes 'DealerHand 'TransactionBoundary = EntityState 'DealerHand (PartialUpdate 'Modes)
    type Rels 'DealerHand 'TransactionBoundary = EntityState 'DealerHand (PartialUpdate 'Rels)

    getAttrs (EDealerHand a _ _) = a
    getModes (EDealerHand _ m _) = m
    getRels (EDealerHand _ _ r) = r

-- DealerRound
instance Decomposable 'DealerRound 'TransactionBoundary where
    type Attrs 'DealerRound 'TransactionBoundary = EntityState 'DealerRound (PartialUpdate 'Attrs)
    type Modes 'DealerRound 'TransactionBoundary = EntityState 'DealerRound (PartialUpdate 'Modes)
    type Rels 'DealerRound 'TransactionBoundary = EntityState 'DealerRound (PartialUpdate 'Rels)

    getAttrs (EDealerRound a _ _) = a
    getModes (EDealerRound _ m _) = m
    getRels (EDealerRound _ _ r) = r

-- Offering
instance Decomposable 'Offering 'TransactionBoundary where
    type Attrs 'Offering 'TransactionBoundary = EntityState 'Offering (PartialUpdate 'Attrs)
    type Modes 'Offering 'TransactionBoundary = EntityState 'Offering (PartialUpdate 'Modes)
    type Rels 'Offering 'TransactionBoundary = EntityState 'Offering (PartialUpdate 'Rels)

    getAttrs (EOffering a _ _) = a
    getModes (EOffering _ m _) = m
    getRels (EOffering _ _ r) = r

-- Player
instance Decomposable 'Player 'TransactionBoundary where
    type Attrs 'Player 'TransactionBoundary = EntityState 'Player (PartialUpdate 'Attrs)
    type Modes 'Player 'TransactionBoundary = EntityState 'Player (PartialUpdate 'Modes)
    type Rels 'Player 'TransactionBoundary = EntityState 'Player (PartialUpdate 'Rels)

    getAttrs (EPlayer a _ _) = a
    getModes (EPlayer _ mo _) = mo
    getRels (EPlayer _ _ r) = r

-- PlayerHand
instance Decomposable 'PlayerHand 'TransactionBoundary where
    type Attrs 'PlayerHand 'TransactionBoundary = EntityState 'PlayerHand (PartialUpdate 'Attrs)
    type Modes 'PlayerHand 'TransactionBoundary = EntityState 'PlayerHand (PartialUpdate 'Modes)
    type Rels 'PlayerHand 'TransactionBoundary = EntityState 'PlayerHand (PartialUpdate 'Rels)

    getAttrs (EPlayerHand a _ _) = a
    getModes (EPlayerHand _ m _) = m
    getRels (EPlayerHand _ _ r) = r

-- PlayerSpot
instance Decomposable 'PlayerSpot 'TransactionBoundary where
    type Attrs 'PlayerSpot 'TransactionBoundary = EntityState 'PlayerSpot (PartialUpdate 'Attrs)
    type Modes 'PlayerSpot 'TransactionBoundary = EntityState 'PlayerSpot (PartialUpdate 'Modes)
    type Rels 'PlayerSpot 'TransactionBoundary = EntityState 'PlayerSpot (PartialUpdate 'Rels)

    getAttrs (EPlayerSpot a _ _) = a
    getModes (EPlayerSpot _ m _) = m
    getRels (EPlayerSpot _ _ r) = r

-- Table
instance Decomposable 'Table 'TransactionBoundary where
    type Attrs 'Table 'TransactionBoundary = EntityState 'Table (PartialUpdate 'Attrs)
    type Modes 'Table 'TransactionBoundary = EntityState 'Table (PartialUpdate 'Modes)
    type Rels 'Table 'TransactionBoundary = EntityState 'Table (PartialUpdate 'Rels)

    getAttrs (ETable a _ _) = a
    getModes (ETable _ m _) = m
    getRels (ETable _ _ r) = r

-- TableShoe
instance Decomposable 'TableShoe 'TransactionBoundary where
    type Attrs 'TableShoe 'TransactionBoundary = EntityState 'TableShoe (PartialUpdate 'Attrs)
    type Modes 'TableShoe 'TransactionBoundary = EntityState 'TableShoe (PartialUpdate 'Modes)
    type Rels 'TableShoe 'TransactionBoundary = EntityState 'TableShoe (PartialUpdate 'Rels)

    getAttrs (ETableShoe a _ _) = a
    getModes (ETableShoe _ m _) = m
    getRels (ETableShoe _ _ r) = r

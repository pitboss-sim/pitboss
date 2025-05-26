{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.State.Entity.Instances.Decomposable (
    Decomposable (..),
) where

import Pitboss.State.Entity.Types

class Decomposable (k :: EntityKind) where
    type Attrs k
    type Modes k
    type Rels k

    getAttrs :: EntityState k -> Attrs k
    getModes :: EntityState k -> Modes k
    getRels :: EntityState k -> Rels k

-- Dealer
instance Decomposable 'Dealer where
    type Attrs 'Dealer = DealerAttrs
    type Modes 'Dealer = DealerModes
    type Rels 'Dealer = DealerRels

    getAttrs (EDealer a _ _) = a
    getModes (EDealer _ m _) = m
    getRels (EDealer _ _ r) = r

-- DealerHand
instance Decomposable 'DealerHand where
    type Attrs 'DealerHand = DealerHandAttrs
    type Modes 'DealerHand = DealerHandModes
    type Rels 'DealerHand = DealerHandRels

    getAttrs (EDealerHand a _ _) = a
    getModes (EDealerHand _ m _) = m
    getRels (EDealerHand _ _ r) = r

-- DealerRound
instance Decomposable 'DealerRound where
    type Attrs 'DealerRound = DealerRoundAttrs
    type Modes 'DealerRound = DealerRoundModes
    type Rels 'DealerRound = DealerRoundRels

    getAttrs (EDealerRound a _ _) = a
    getModes (EDealerRound _ m _) = m
    getRels (EDealerRound _ _ r) = r

-- Offering
instance Decomposable 'Offering where
    type Attrs 'Offering = OfferingAttrs
    type Modes 'Offering = OfferingModes
    type Rels 'Offering = OfferingRels

    getAttrs (EOffering a _ _) = a
    getModes (EOffering _ m _) = m
    getRels (EOffering _ _ r) = r

-- Player
instance Decomposable 'Player where
    type Attrs 'Player = PlayerAttrs
    type Modes 'Player = PlayerModes
    type Rels 'Player = PlayerRels

    getAttrs (EPlayer a _ _) = a
    getModes (EPlayer _ mo _) = mo
    getRels (EPlayer _ _ r) = r

-- PlayerHand
instance Decomposable 'PlayerHand where
    type Attrs 'PlayerHand = PlayerHandAttrs
    type Modes 'PlayerHand = PlayerHandModes
    type Rels 'PlayerHand = PlayerHandRels

    getAttrs (EPlayerHand a _ _) = a
    getModes (EPlayerHand _ m _) = m
    getRels (EPlayerHand _ _ r) = r

-- PlayerSpot
instance Decomposable 'PlayerSpot where
    type Attrs 'PlayerSpot = PlayerSpotAttrs
    type Modes 'PlayerSpot = PlayerSpotModes
    type Rels 'PlayerSpot = PlayerSpotRels

    getAttrs (EPlayerSpot a _ _) = a
    getModes (EPlayerSpot _ m _) = m
    getRels (EPlayerSpot _ _ r) = r

-- Table
instance Decomposable 'Table where
    type Attrs 'Table = TableAttrs
    type Modes 'Table = TableModes
    type Rels 'Table = TableRels

    getAttrs (ETable a _ _) = a
    getModes (ETable _ m _) = m
    getRels (ETable _ _ r) = r

-- TableShoe
instance Decomposable 'TableShoe where
    type Attrs 'TableShoe = TableShoeAttrs
    type Modes 'TableShoe = TableShoeModes
    type Rels 'TableShoe = TableShoeRels

    getAttrs (ETableShoe a _ _) = a
    getModes (ETableShoe _ m _) = m
    getRels (ETableShoe _ _ r) = r

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.Capability.Extractable where

import Pitboss.Trace.Entity.Dealer.Entity
import Pitboss.Trace.Entity.DealerHand.Entity
import Pitboss.Trace.Entity.DealerRound.Entity
import Pitboss.Trace.Entity.Entity

class Extractable (k :: EntityKind) where
    type Attrs k
    type Modes k
    type Rels k
    type IdOf k

    getMeta :: Entity k -> Meta (ClockedRef (IdOf k))
    getAttrs :: Entity k -> Attrs k
    getModes :: Entity k -> Modes k
    getRels :: Entity k -> Rels k

instance Extractable 'Dealer where
    type Attrs Dealer = EDealerAttrs
    type Modes Dealer = EDealerModes
    type Rels Dealer = EDealerRels
    type IdOf Dealer = EDealerId

    getMeta (EDealer m _ _ _) = m
    getAttrs (EDealer _ a _ _) = a
    getModes (EDealer _ _ m _) = m
    getRels (EDealer _ _ _ r) = r

instance Extractable DealerHand where
    type Attrs DealerHand = EDealerHandAttrs
    type Modes DealerHand = EDealerHandModes
    type Rels DealerHand = EDealerHandRels
    type IdOf DealerHand = EDealerHandId

    getMeta (EDealerHand m _ _ _) = m
    getAttrs (EDealerHand _ a _ _) = a
    getModes (EDealerHand _ _ m _) = m
    getRels (EDealerHand _ _ _ r) = r

instance Extractable DealerRound where
    type Attrs DealerRound = EDealerRoundAttrs
    type Modes DealerRound = EDealerRoundModes
    type Rels DealerRound = EDealerRoundRels
    type IdOf DealerRound = EDealerRoundId

    getMeta (EDealerRound m _ _ _) = m
    getAttrs (EDealerRound _ a _ _) = a
    getModes (EDealerRound _ _ m _) = m
    getRels (EDealerRound _ _ _ r) = r

instance Extractable Offering where
    type Attrs Offering = EOfferingAttrs
    type Modes Offering = EOfferingModes
    type Rels Offering = EOfferingRels
    type IdOf Offering = EOfferingId

    getMeta (EOffering m _ _ _) = m
    getAttrs (EOffering _ a _ _) = a
    getModes (EOffering _ _ m _) = m
    getRels (EOffering _ _ _ r) = r

instance Extractable Player where
    type Attrs Player = EPlayerAttrs
    type Modes Player = EPlayerModes
    type Rels Player = EPlayerRels
    type IdOf Player = EPlayerId

    getMeta (EPlayer m _ _ _) = m
    getAttrs (EPlayer _ a _ _) = a
    getModes (EPlayer _ _ m _) = m
    getRels (EPlayer _ _ _ r) = r

instance Extractable PlayerHand where
    type Attrs PlayerHand = EPlayerHandAttrs
    type Modes PlayerHand = EPlayerHandModes
    type Rels PlayerHand = EPlayerHandRels
    type IdOf PlayerHand = EPlayerHandId

    getMeta (EPlayerHand m _ _ _) = m
    getAttrs (EPlayerHand _ a _ _) = a
    getModes (EPlayerHand _ _ m _) = m
    getRels (EPlayerHand _ _ _ r) = r

instance Extractable PlayerSpot where
    type Attrs PlayerSpot = EPlayerSpotAttrs
    type Modes PlayerSpot = EPlayerSpotModes
    type Rels PlayerSpot = EPlayerSpotRels
    type IdOf PlayerSpot = EPlayerSpotId

    getMeta (EPlayerSpot m _ _ _) = m
    getAttrs (EPlayerSpot _ a _ _) = a
    getModes (EPlayerSpot _ _ m _) = m
    getRels (EPlayerSpot _ _ _ r) = r

instance Extractable Table where
    type Attrs Table = ETableAttrs
    type Modes Table = ETableModes
    type Rels Table = ETableRels
    type IdOf Table = ETableId

    getMeta (ETable m _ _ _) = m
    getAttrs (ETable _ a _ _) = a
    getModes (ETable _ _ m _) = m
    getRels (ETable _ _ _ r) = r

instance Extractable TableShoe where
    type Attrs TableShoe = ETableShoeAttrs
    type Modes TableShoe = ETableShoeModes
    type Rels TableShoe = ETableShoeRels
    type IdOf TableShoe = ETableShoeId

    getMeta (ETableShoe m _ _ _) = m
    getAttrs (ETableShoe _ a _ _) = a
    getModes (ETableShoe _ _ m _) = m
    getRels (ETableShoe _ _ _ r) = r

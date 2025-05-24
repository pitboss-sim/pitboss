{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.Capability.Extractable where

import Pitboss.Trace.Entity.Dealer.Entity
import Pitboss.Trace.Entity.DealerHand.Entity
import Pitboss.Trace.Entity.DealerRound.Entity
import Pitboss.Trace.Entity.Entity
import Pitboss.Trace.Entity.Offering.Entity
import Pitboss.Trace.Entity.Player.Entity
import Pitboss.Trace.Entity.PlayerHand.Entity
import Pitboss.Trace.Entity.PlayerSpot.Entity
import Pitboss.Trace.Entity.Table.Entity
import Pitboss.Trace.Entity.TableShoe.Entity
import Pitboss.Trace.Entity.Types
import Pitboss.Trace.Entity.Types.EntityId

class Extractable (k :: EntityKind) where
    type Attrs k
    type Modes k
    type Rels k
    type IdOf k

    getMeta :: Entity k -> Meta (ClockedRef (IdOf k))
    getAttrs :: Entity k -> Attrs k
    getModes :: Entity k -> Modes k
    getRels :: Entity k -> Rels k

instance Extractable 'DealerEntity where
    type Attrs 'DealerEntity = DealerEntityAttrs
    type Modes 'DealerEntity = DealerEntityModes
    type Rels 'DealerEntity = DealerEntityRels
    type IdOf 'DealerEntity = DealerEntityId

    getMeta (DealerEntity' m _ _ _) = m
    getAttrs (DealerEntity' _ a _ _) = a
    getModes (DealerEntity' _ _ m _) = m
    getRels (DealerEntity' _ _ _ r) = r

instance Extractable 'DealerHandEntity where
    type Attrs 'DealerHandEntity = DealerHandEntityAttrs
    type Modes 'DealerHandEntity = DealerHandEntityModes
    type Rels 'DealerHandEntity = DealerHandEntityRels
    type IdOf 'DealerHandEntity = DealerHandEntityId

    getMeta (DealerHandEntity' m _ _ _) = m
    getAttrs (DealerHandEntity' _ a _ _) = a
    getModes (DealerHandEntity' _ _ m _) = m
    getRels (DealerHandEntity' _ _ _ r) = r

instance Extractable 'DealerRoundEntity where
    type Attrs 'DealerRoundEntity = DealerRoundEntityAttrs
    type Modes 'DealerRoundEntity = DealerRoundEntityModes
    type Rels 'DealerRoundEntity = DealerRoundEntityRels
    type IdOf 'DealerRoundEntity = DealerRoundEntityId

    getMeta (DealerRoundEntity' m _ _ _) = m
    getAttrs (DealerRoundEntity' _ a _ _) = a
    getModes (DealerRoundEntity' _ _ m _) = m
    getRels (DealerRoundEntity' _ _ _ r) = r

instance Extractable 'OfferingEntity where
    type Attrs 'OfferingEntity = OfferingEntityAttrs
    type Modes 'OfferingEntity = OfferingEntityModes
    type Rels 'OfferingEntity = OfferingEntityRels
    type IdOf 'OfferingEntity = OfferingEntityId

    getMeta (OfferingEntity' m _ _ _) = m
    getAttrs (OfferingEntity' _ a _ _) = a
    getModes (OfferingEntity' _ _ m _) = m
    getRels (OfferingEntity' _ _ _ r) = r

instance Extractable 'PlayerEntity where
    type Attrs 'PlayerEntity = PlayerEntityAttrs
    type Modes 'PlayerEntity = PlayerEntityModes
    type Rels 'PlayerEntity = PlayerEntityRels
    type IdOf 'PlayerEntity = PlayerEntityId

    getMeta (PlayerEntity' m _ _ _) = m
    getAttrs (PlayerEntity' _ a _ _) = a
    getModes (PlayerEntity' _ _ m _) = m
    getRels (PlayerEntity' _ _ _ r) = r

instance Extractable 'PlayerHandEntity where
    type Attrs 'PlayerHandEntity = PlayerHandEntityAttrs
    type Modes 'PlayerHandEntity = PlayerHandEntityModes
    type Rels 'PlayerHandEntity = PlayerHandEntityRels
    type IdOf 'PlayerHandEntity = PlayerHandEntityId

    getMeta (PlayerHandEntity' m _ _ _) = m
    getAttrs (PlayerHandEntity' _ a _ _) = a
    getModes (PlayerHandEntity' _ _ m _) = m
    getRels (PlayerHandEntity' _ _ _ r) = r

instance Extractable 'PlayerSpotEntity where
    type Attrs 'PlayerSpotEntity = PlayerSpotEntityAttrs
    type Modes 'PlayerSpotEntity = PlayerSpotEntityModes
    type Rels 'PlayerSpotEntity = PlayerSpotEntityRels
    type IdOf 'PlayerSpotEntity = PlayerSpotEntityId

    getMeta (PlayerSpotEntity' m _ _ _) = m
    getAttrs (PlayerSpotEntity' _ a _ _) = a
    getModes (PlayerSpotEntity' _ _ m _) = m
    getRels (PlayerSpotEntity' _ _ _ r) = r

instance Extractable 'TableEntity where
    type Attrs 'TableEntity = TableEntityAttrs
    type Modes 'TableEntity = TableEntityModes
    type Rels 'TableEntity = TableEntityRels
    type IdOf 'TableEntity = TableEntityId

    getMeta (TableEntity' m _ _ _) = m
    getAttrs (TableEntity' _ a _ _) = a
    getModes (TableEntity' _ _ m _) = m
    getRels (TableEntity' _ _ _ r) = r

instance Extractable 'TableShoeEntity where
    type Attrs 'TableShoeEntity = TableShoeEntityAttrs
    type Modes 'TableShoeEntity = TableShoeEntityModes
    type Rels 'TableShoeEntity = TableShoeEntityRels
    type IdOf 'TableShoeEntity = TableShoeEntityId

    getMeta (TableShoeEntity' m _ _ _) = m
    getAttrs (TableShoeEntity' _ a _ _) = a
    getModes (TableShoeEntity' _ _ m _) = m
    getRels (TableShoeEntity' _ _ _ r) = r

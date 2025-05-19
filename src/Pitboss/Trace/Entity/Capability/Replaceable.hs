{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Pitboss.Trace.Entity.Capability.Replaceable where

import Pitboss.Trace.Entity.Capability.Extractable
import Pitboss.Trace.Entity.Entity
import Pitboss.Trace.Entity.Types

class Replaceable (k :: EntityKind) where
    replaceAttrs :: Entity k -> Attrs k -> Entity k
    replaceModes :: Entity k -> Modes k -> Entity k
    replaceRels :: Entity k -> Rels k -> Entity k

instance Replaceable 'DealerEntity where
    replaceAttrs (DealerEntity' m _ mo r) a = DealerEntity' m a mo r
    replaceModes (DealerEntity' m a _ r) mo = DealerEntity' m a mo r
    replaceRels (DealerEntity' m a mo _) r = DealerEntity' m a mo r

instance Replaceable 'DealerHandEntity where
    replaceAttrs (DealerHandEntity' m _ mo r) a = DealerHandEntity' m a mo r
    replaceModes (DealerHandEntity' m a _ r) mo = DealerHandEntity' m a mo r
    replaceRels (DealerHandEntity' m a mo _) r = DealerHandEntity' m a mo r

instance Replaceable 'DealerRoundEntity where
    replaceAttrs (DealerRoundEntity' m _ mo r) a = DealerRoundEntity' m a mo r
    replaceModes (DealerRoundEntity' m a _ r) mo = DealerRoundEntity' m a mo r
    replaceRels (DealerRoundEntity' m a mo _) r = DealerRoundEntity' m a mo r

instance Replaceable 'OfferingEntity where
    replaceAttrs (OfferingEntity' m _ mo r) a = OfferingEntity' m a mo r
    replaceModes (OfferingEntity' m a _ r) mo = OfferingEntity' m a mo r
    replaceRels (OfferingEntity' m a mo _) r = OfferingEntity' m a mo r

instance Replaceable 'PlayerEntity where
    replaceAttrs (PlayerEntity' m _ mo r) a = PlayerEntity' m a mo r
    replaceModes (PlayerEntity' m a _ r) mo = PlayerEntity' m a mo r
    replaceRels (PlayerEntity' m a mo _) r = PlayerEntity' m a mo r

instance Replaceable 'PlayerHandEntity where
    replaceAttrs (PlayerHandEntity' m _ mo r) a = PlayerHandEntity' m a mo r
    replaceModes (PlayerHandEntity' m a _ r) mo = PlayerHandEntity' m a mo r
    replaceRels (PlayerHandEntity' m a mo _) r = PlayerHandEntity' m a mo r

instance Replaceable 'PlayerSpotEntity where
    replaceAttrs (PlayerSpotEntity' m _ mo r) a = PlayerSpotEntity' m a mo r
    replaceModes (PlayerSpotEntity' m a _ r) mo = PlayerSpotEntity' m a mo r
    replaceRels (PlayerSpotEntity' m a mo _) r = PlayerSpotEntity' m a mo r

instance Replaceable 'TableEntity where
    replaceAttrs (TableEntity' m _ mo r) a = TableEntity' m a mo r
    replaceModes (TableEntity' m a _ r) mo = TableEntity' m a mo r
    replaceRels (TableEntity' m a mo _) r = TableEntity' m a mo r

instance Replaceable 'TableShoeEntity where
    replaceAttrs (TableShoeEntity' m _ mo r) a = TableShoeEntity' m a mo r
    replaceModes (TableShoeEntity' m a _ r) mo = TableShoeEntity' m a mo r
    replaceRels (TableShoeEntity' m a mo _) r = TableShoeEntity' m a mo r

instance Replaceable 'TableShoeCursorEntity where
    replaceAttrs (TableShoeCursorEntity' m _ mo r) a = TableShoeCursorEntity' m a mo r
    replaceModes (TableShoeCursorEntity' m a _ r) mo = TableShoeCursorEntity' m a mo r
    replaceRels (TableShoeCursorEntity' m a mo _) r = TableShoeCursorEntity' m a mo r

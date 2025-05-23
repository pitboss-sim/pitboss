{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Pitboss.Trace.Entity.Capability.Replaceable where

import Pitboss.Trace.Entity.Capability.Extractable
import Pitboss.Trace.Entity.Entity

class Replaceable (k :: EntityKind) where
    replaceAttrs :: Entity k -> Attrs k -> Entity k
    replaceModes :: Entity k -> Modes k -> Entity k
    replaceRels :: Entity k -> Rels k -> Entity k

instance Replaceable 'Dealer where
    replaceAttrs (EDealer m _ mo r) a = EDealer m a mo r
    replaceModes (EDealer m a _ r) mo = EDealer m a mo r
    replaceRels (EDealer m a mo _) r = EDealer m a mo r

instance Replaceable 'DealerHand where
    replaceAttrs (EDealerHand m _ mo r) a = EDealerHand m a mo r
    replaceModes (EDealerHand m a _ r) mo = EDealerHand m a mo r
    replaceRels (EDealerHand m a mo _) r = EDealerHand m a mo r

instance Replaceable 'DealerRound where
    replaceAttrs (EDealerRound m _ mo r) a = EDealerRound m a mo r
    replaceModes (EDealerRound m a _ r) mo = EDealerRound m a mo r
    replaceRels (EDealerRound m a mo _) r = EDealerRound m a mo r

instance Replaceable 'Offering where
    replaceAttrs (EOffering m _ mo r) a = EOffering m a mo r
    replaceModes (EOffering m a _ r) mo = EOffering m a mo r
    replaceRels (EOffering m a mo _) r = EOffering m a mo r

instance Replaceable 'Player where
    replaceAttrs (EPlayer m _ mo r) a = EPlayer m a mo r
    replaceModes (EPlayer m a _ r) mo = EPlayer m a mo r
    replaceRels (EPlayer m a mo _) r = EPlayer m a mo r

instance Replaceable 'PlayerHand where
    replaceAttrs (EPlayerHand m _ mo r) a = EPlayerHand m a mo r
    replaceModes (EPlayerHand m a _ r) mo = EPlayerHand m a mo r
    replaceRels (EPlayerHand m a mo _) r = EPlayerHand m a mo r

instance Replaceable 'PlayerSpot where
    replaceAttrs (EPlayerSpot m _ mo r) a = EPlayerSpot m a mo r
    replaceModes (EPlayerSpot m a _ r) mo = EPlayerSpot m a mo r
    replaceRels (EPlayerSpot m a mo _) r = EPlayerSpot m a mo r

instance Replaceable 'Table where
    replaceAttrs (ETable m _ mo r) a = ETable m a mo r
    replaceModes (ETable m a _ r) mo = ETable m a mo r
    replaceRels (ETable m a mo _) r = ETable m a mo r

instance Replaceable 'TableShoe where
    replaceAttrs (ETableShoe m _ mo r) a = ETableShoe m a mo r
    replaceModes (ETableShoe m a _ r) mo = ETableShoe m a mo r
    replaceRels (ETableShoe m a mo _) r = ETableShoe m a mo r

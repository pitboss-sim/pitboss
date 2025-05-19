{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Pitboss.Causality.Entity.Witnessable where

import Pitboss.Causality.Entity.Types
import Pitboss.Causality.Types.Core

class Witnessable (k :: EntityKind) where
    witness :: EntityState k -> EntityKindWitness k

instance Witnessable 'Bout where
    witness _ = BoutWitness

instance Witnessable 'Dealer where
    witness _ = DealerWitness

instance Witnessable 'DealerHand where
    witness _ = DealerHandWitness

instance Witnessable 'DealerRound where
    witness _ = DealerRoundWitness

instance Witnessable 'Player where
    witness _ = PlayerWitness

instance Witnessable 'PlayerHand where
    witness _ = PlayerHandWitness

instance Witnessable 'PlayerSpot where
    witness _ = PlayerSpotWitness

instance Witnessable 'Table where
    witness _ = TableWitness

instance Witnessable 'TableShoe where
    witness _ = TableShoeWitness

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

instance Witnessable 'Player where
    witness _ = PlayerWitness

instance Witnessable 'Round where
    witness _ = RoundWitness

instance Witnessable 'Shoe where
    witness _ = ShoeWitness

instance Witnessable 'Table where
    witness _ = TableWitness

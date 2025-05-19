{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Pitboss.Causality.Entity.Instances.Witnessable where

import Pitboss.Causality.Entity.Types
import Pitboss.Causality.Types.Core

class Witnessable (k :: EntityKind) where
    witness :: EntityState k -> EntityKindWitness k

data EntityKindWitness (k :: EntityKind) where
    BoutWitness :: EntityKindWitness 'Bout
    PlayerWitness :: EntityKindWitness 'Player
    DealerWitness :: EntityKindWitness 'Dealer
    PlayerHandWitness :: EntityKindWitness 'PlayerHand
    DealerHandWitness :: EntityKindWitness 'DealerHand
    PlayerSpotWitness :: EntityKindWitness 'PlayerSpot
    DealerRoundWitness :: EntityKindWitness 'DealerRound
    TableWitness :: EntityKindWitness 'Table
    TableShoeWitness :: EntityKindWitness 'TableShoe

instance Show (EntityKindWitness k) where
    show BoutWitness = "BoutWitness"
    show PlayerWitness = "PlayerWitness"
    show DealerWitness = "DealerWitness"
    show PlayerHandWitness = "PlayerHandWitness"
    show DealerHandWitness = "DealerHandWitness"
    show PlayerSpotWitness = "PlayerSpotWitness"
    show DealerRoundWitness = "DealerRoundWitness"
    show TableWitness = "TableWitness"
    show TableShoeWitness = "TableShoeWitness"

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

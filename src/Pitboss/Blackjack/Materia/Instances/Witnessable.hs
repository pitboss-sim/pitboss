{-# LANGUAGE DataKinds #-}

module Pitboss.Blackjack.Materia.Instances.Witnessable where

import Pitboss.Blackjack.Materia.Types.Core

class Witnessable (k :: HandKind) where
    witness :: Hand k -> HandKindWitness k

instance Witnessable 'BlackjackHand where
    witness _ = BlackjackWitness

instance Witnessable 'TwentyOneHand where
    witness _ = TwentyOneWitness

instance Witnessable 'SoftHand where
    witness _ = SoftWitness

instance Witnessable 'HardHand where
    witness _ = HardWitness

instance Witnessable 'PairHand where
    witness _ = PairWitness

instance Witnessable 'BustHand where
    witness _ = BustWitness

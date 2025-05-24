{-# LANGUAGE DataKinds #-}

module Pitboss.Causality.Trace.Instances.Registrable where

import Control.Lens (Lens')
import Pitboss.Causality.Delta.Types
import Pitboss.Causality.Registry
import Pitboss.Causality.Trace
import Pitboss.Causality.Types.Core

class Registrable (k :: EntityKind) where
    registryLens :: Lens' Trace (Registry k (SomeDelta k))

instance Registrable 'Bout where
    registryLens = bouts

instance Registrable 'Table where
    registryLens = tables

instance Registrable 'TableShoe where
    registryLens = tableShoes

instance Registrable 'Dealer where
    registryLens = dealers

instance Registrable 'DealerHand where
    registryLens = dealerHands

instance Registrable 'DealerRound where
    registryLens = dealerRounds

instance Registrable 'Player where
    registryLens = players

instance Registrable 'PlayerSpot where
    registryLens = playerSpots

instance Registrable 'PlayerHand where
    registryLens = playerHands

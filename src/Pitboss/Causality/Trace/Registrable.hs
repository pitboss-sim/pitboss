{-# LANGUAGE DataKinds #-}

module Pitboss.Causality.Trace.Registrable where

import Control.Lens (Lens')
import Data.Hashable (Hashable)
import Pitboss.Causality.Delta.Types
import Pitboss.Causality.Registry
import Pitboss.Causality.Trace
import Pitboss.Causality.Types.Core

class (Hashable (EntityIdFor k)) => Registrable (k :: EntityKind) where
    registryLens :: Lens' Trace (Registry k (SomeDelta k))

instance Registrable 'Bout where
    registryLens = bouts

instance Registrable 'Dealer where
    registryLens = dealers

instance Registrable 'Player where
    registryLens = players

instance Registrable 'Round where
    registryLens = rounds

instance Registrable 'Shoe where
    registryLens = shoes

instance Registrable 'Table where
    registryLens = tables

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Pitboss.Causality.Delta.IncrementalPart where

import Pitboss.Causality.Delta.Incremental
import Pitboss.Causality.Delta.Types
import Pitboss.Causality.Entity.Types
import Pitboss.Causality.Types.Core

data PartWitness (s :: EntityStatePart) where
    AttrsWitness :: PartWitness 'Attrs
    ModesWitness :: PartWitness 'Modes
    RelsWitness :: PartWitness 'Rels

class IncrementalPart k where
    applyPart ::
        PartWitness s ->
        Delta k (PartialUpdate s) ->
        EntityState k ->
        EntityState k

instance IncrementalPart 'Bout where
    applyPart AttrsWitness delta (EBout attrs modes rels) =
        EBout (apply delta attrs) modes rels
    applyPart ModesWitness delta (EBout attrs modes rels) =
        EBout attrs (apply delta modes) rels
    applyPart RelsWitness delta (EBout attrs modes rels) =
        EBout attrs modes (apply delta rels)

instance IncrementalPart 'Dealer where
    applyPart AttrsWitness delta (EDealer attrs modes rels) =
        EDealer (apply delta attrs) modes rels
    applyPart ModesWitness delta (EDealer attrs modes rels) =
        EDealer attrs (apply delta modes) rels
    applyPart RelsWitness delta (EDealer attrs modes rels) =
        EDealer attrs modes (apply delta rels)

instance IncrementalPart 'Player where
    applyPart AttrsWitness delta (EPlayer attrs modes rels) =
        EPlayer (apply delta attrs) modes rels
    applyPart ModesWitness delta (EPlayer attrs modes rels) =
        EPlayer attrs (apply delta modes) rels
    applyPart RelsWitness delta (EPlayer attrs modes rels) =
        EPlayer attrs modes (apply delta rels)

instance IncrementalPart 'Round where
    applyPart AttrsWitness delta (ERound attrs modes rels) =
        ERound (apply delta attrs) modes rels
    applyPart ModesWitness delta (ERound attrs modes rels) =
        ERound attrs (apply delta modes) rels
    applyPart RelsWitness delta (ERound attrs modes rels) =
        ERound attrs modes (apply delta rels)

instance IncrementalPart 'Shoe where
    applyPart AttrsWitness delta (EShoe attrs modes rels) =
        EShoe (apply delta attrs) modes rels
    applyPart ModesWitness delta (EShoe attrs modes rels) =
        EShoe attrs (apply delta modes) rels
    applyPart RelsWitness delta (EShoe attrs modes rels) =
        EShoe attrs modes (apply delta rels)

instance IncrementalPart 'Table where
    applyPart AttrsWitness delta (ETable attrs modes rels) =
        ETable (apply delta attrs) modes rels
    applyPart ModesWitness delta (ETable attrs modes rels) =
        ETable attrs (apply delta modes) rels
    applyPart RelsWitness delta (ETable attrs modes rels) =
        ETable attrs modes (apply delta rels)

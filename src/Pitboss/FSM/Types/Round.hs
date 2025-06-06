{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.FSM.Types.Round where

import Data.Aeson.Types
import GHC.Generics (Generic)
import Pitboss.FSM.Types.Core

data RoundFSM
    = PeekRound SomePeekFSM
    | ENHCRound SomeENHCFSM
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

mkENHCRound :: ENHCFSM p -> RoundFSM
mkENHCRound = ENHCRound . SomeENHCFSM

mkPeekRound :: PeekFSM p -> RoundFSM
mkPeekRound = PeekRound . SomePeekFSM

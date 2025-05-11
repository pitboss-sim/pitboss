{-# LANGUAGE DerivingStrategies #-}

module Pitboss.Trace where

import GHC.Generics (Generic)

-- global simulation state

data Trace = Trace
  {
  }
  deriving stock (Generic, Show)

emptyTrace :: Trace
emptyTrace =
  Trace
    {
    }

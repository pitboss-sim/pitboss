{-# LANGUAGE GADTs #-}

module Pitboss.Trace.Types.EntityRef where

import Data.Aeson
import GHC.Generics
import Pitboss.Trace.Entity.Capabilities

data EntityRef id
  = TimelessRef id
  | ClockedRef Tick id
  deriving (Eq, Show, Generic)

instance (ToJSON id, Generic id) => ToJSON (EntityRef id)

instance (FromJSON id, Generic id) => FromJSON (EntityRef id)

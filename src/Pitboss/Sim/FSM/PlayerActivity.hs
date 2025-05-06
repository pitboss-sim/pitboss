{-# LANGUAGE DeriveGeneric #-}

module Pitboss.Sim.FSM.PlayerActivity where

import GHC.Generics (Generic)

data PlayerActivity
  = Idle -- traipsing
  | Waiting [TableId] -- queueing for specific tables
  | Seated TableId -- participating with table
  deriving (Eq, Show, Generic)

-- Transitions
wait :: [TableId] -> PlayerActivity -> PlayerActivity
wait tids _ = Waiting tids

sitAt :: TableId -> PlayerActivity -> PlayerActivity
sitAt tid (Waiting ts)
  | tid `elem` ts = Playing tid
  | otherwise = Playing tid
sitAt tid _ = Playing tid

leave :: PlayerActivity -> PlayerActivity
leave _ = Idle

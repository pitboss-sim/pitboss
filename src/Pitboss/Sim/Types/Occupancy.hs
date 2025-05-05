module Pitboss.Sim.Types.Occupancy where

data Occupancy a
  = Absent
  | Present a
  deriving (Eq, Show, Functor)

isPresent :: Occupancy a -> Bool
isPresent (Present _) = True
isPresent Absent = False

isAbsent :: Occupancy a -> Bool
isAbsent = not . isPresent

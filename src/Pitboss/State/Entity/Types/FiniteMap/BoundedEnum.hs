module Pitboss.State.Entity.Types.FiniteMap.BoundedEnum where

class (Enum a, Bounded a) => BoundedEnum a

universe :: (BoundedEnum a) => [a]
universe = [minBound .. maxBound]

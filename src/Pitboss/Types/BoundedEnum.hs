module Pitboss.Types.BoundedEnum
  ( BoundedEnum,
    universe,
  )
where

class (Enum a, Bounded a) => BoundedEnum a

universe :: (BoundedEnum a) => [a]
universe = [minBound .. maxBound]

module Pitboss.Trace.Entity.Capabilities.Reversible where

class Reversible d where
  invert :: d -> Maybe d

data InversionError
  = NotInvertible
  | MissingPriorContext String
  | CustomReason String
  deriving (Eq, Show)

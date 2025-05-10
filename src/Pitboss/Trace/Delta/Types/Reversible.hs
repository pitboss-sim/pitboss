module Pitboss.Trace.Delta.Types.Reversible where

class Reversible d where
  invert :: d -> Maybe d

data InversionError
  = NotInvertible -- one-way operation
  | MissingPriorContext String -- need prior state to reverse
  | CustomReason String -- general fallback
  deriving (Eq, Show)

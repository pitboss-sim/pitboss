{-# LANGUAGE LambdaCase #-}

module Pitboss.Trace.Entity.Capabilities.Reversible where

import Pitboss.Trace.Entity.PlayerHand

class Reversible d where
  invert :: d -> Maybe d

data InversionError
  = NotInvertible
  | MissingPriorContext String
  | CustomReason String
  deriving (Eq, Show)

instance Reversible PlayerHandDelta where
  invert = \case
    AddCard c -> Just (RemoveCard c)
    RemoveCard c -> Just (AddCard c)
    ReplaceCards old new -> Just (ReplaceCards new old)
    ReplacePlayerHandIndex from to -> Just (ReplacePlayerHandIndex to from)
    ReplaceSplitDepth from to -> Just (ReplaceSplitDepth to from)

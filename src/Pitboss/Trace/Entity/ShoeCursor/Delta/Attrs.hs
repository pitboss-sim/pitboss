{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.ShoeCursor.Delta.Attrs where

import Data.Aeson
import GHC.Generics
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.ShoeCursor

data ShoeCursorStateDelta
  = Advance Int
  | Rewind Int
  | ReplaceOffset Int Int
  deriving (Eq, Show, Generic)

instance ToJSON ShoeCursorStateDelta

instance FromJSON ShoeCursorStateDelta

instance Incremental ShoeCursorStateDelta where
  type Entity ShoeCursorStateDelta = ShoeCursorState

  applyDelta delta state = case delta of
    Advance n -> state {_shoeCursorStateOffset = _shoeCursorStateOffset state + n}
    Rewind n -> state {_shoeCursorStateOffset = _shoeCursorStateOffset state - n}
    ReplaceOffset _ new -> state {_shoeCursorStateOffset = new}

  previewDelta delta entity = Just $ applyDelta delta entity

  describeDelta delta _ = case delta of
    Advance n -> "Advanced cursor by " ++ show n
    Rewind n -> "Rewound cursor by " ++ show n
    ReplaceOffset old new -> "Offset: " ++ show old ++ " → " ++ show new

instance Reversible ShoeCursorStateDelta where
  invert = \case
    Advance n -> Right (Rewind n)
    Rewind n -> Right (Advance n)
    ReplaceOffset old new -> Right (ReplaceOffset new old)

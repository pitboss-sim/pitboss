{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.TableShoeCursor.Delta.Attrs where

import Data.Aeson
import GHC.Generics
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.TableShoeCursor

data TableShoeCursorEntityAttrsDelta
  = Advance Int
  | Rewind Int
  | ReplaceOffset Int Int
  deriving (Eq, Show, Generic)

instance ToJSON TableShoeCursorEntityAttrsDelta

instance FromJSON TableShoeCursorEntityAttrsDelta

instance Incremental TableShoeCursorEntityAttrsDelta where
  type Target TableShoeCursorEntityAttrsDelta = TableShoeCursorEntityAttrs

  applyDelta delta state = case delta of
    Advance n -> state {_tableShoeCursorEntityAttrsOffset = _tableShoeCursorEntityAttrsOffset state + n}
    Rewind n -> state {_tableShoeCursorEntityAttrsOffset = _tableShoeCursorEntityAttrsOffset state - n}
    ReplaceOffset _ new -> state {_tableShoeCursorEntityAttrsOffset = new}

  previewDelta delta entity = Just $ applyDelta delta entity

  describeDelta delta _ = case delta of
    Advance n -> "Advanced cursor by " ++ show n
    Rewind n -> "Rewound cursor by " ++ show n
    ReplaceOffset old new -> "Offset: " ++ show old ++ " → " ++ show new

instance Reversible TableShoeCursorEntityAttrsDelta where
  invert = \case
    Advance n -> Right (Rewind n)
    Rewind n -> Right (Advance n)
    ReplaceOffset old new -> Right (ReplaceOffset new old)

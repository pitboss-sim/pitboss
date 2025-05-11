{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Pitboss.Trace.Entity.ShoeCursor where

import GHC.Generics (Generic)
import Pitboss.Trace.Entity.Types.Meta
import Pitboss.Trace.Types.Identifier

mkShoeCursor :: Meta ShoeCursorId -> ShoeCursorRelations -> ShoeCursorState -> ShoeCursor
mkShoeCursor = ShoeCursor

mkShoeCursorState :: Int -> ShoeCursorState
mkShoeCursorState = ShoeCursorState

mkShoeCursorRelations :: ShoeId -> ShoeCursorRelations
mkShoeCursorRelations = ShoeCursorRelations

data ShoeCursor = ShoeCursor
  { _meta :: Meta ShoeCursorId,
    _rels :: ShoeCursorRelations,
    _state :: ShoeCursorState
  }
  deriving (Eq, Show, Generic)

data ShoeCursorState = ShoeCursorState
  { _offset :: Int
  }
  deriving (Eq, Show, Generic)

data ShoeCursorRelations = ShoeCursorRelations
  { _pointsToShoe :: ShoeId
  }
  deriving (Eq, Show, Generic)

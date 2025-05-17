{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Pitboss.Trace.Entity.ShoeCursor where

import GHC.Generics (Generic)
import Pitboss.Trace.Entity.Types.Meta
import Pitboss.Trace.Types.EntityRef
import Pitboss.Trace.Types.Identifier

mkShoeCursor :: Meta (EntityRef ShoeCursorId) -> ShoeCursorRelations -> ShoeCursorState -> ShoeCursor
mkShoeCursor = ShoeCursor

mkShoeCursorState :: Int -> ShoeCursorState
mkShoeCursorState = ShoeCursorState

mkShoeCursorRelations :: EntityRef ShoeId -> ShoeCursorRelations
mkShoeCursorRelations = ShoeCursorRelations

data ShoeCursor = ShoeCursor
  { _shoeCursorMeta :: Meta (EntityRef ShoeCursorId),
    _shoeCursorRels :: ShoeCursorRelations,
    _shoeCursorState :: ShoeCursorState
  }
  deriving (Eq, Show, Generic)

data ShoeCursorState = ShoeCursorState
  { _shoeCursorStateOffset :: Int
  }
  deriving (Eq, Show, Generic)

data ShoeCursorRelations = ShoeCursorRelations
  { _shoeCursorRelsPointsToShoe :: EntityRef ShoeId
  }
  deriving (Eq, Show, Generic)

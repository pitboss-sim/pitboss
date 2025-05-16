{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Delta.Capabilities.Incremental where

import Pitboss.Trace.Types.Uid

class Incremental delta where
  type Entity delta

  applyDelta :: delta -> Entity delta -> Entity delta
  previewDelta :: delta -> Entity delta -> Maybe (Entity delta)
  describeDelta :: delta -> Entity delta -> String

class (Incremental delta) => Identifiable delta where
  entityToId :: delta -> Entity delta -> Uid

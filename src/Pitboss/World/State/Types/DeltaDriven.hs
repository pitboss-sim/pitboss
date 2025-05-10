{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}

module Pitboss.World.State.Types.DeltaDriven where

class DeltaDriven entity delta | entity -> delta where
  applyDelta :: delta -> entity -> entity
  previewDelta :: delta -> entity -> Maybe entity
  describeDelta :: delta -> entity -> String

data TimedDelta d t = TimedDelta {timestamp :: t, delta :: d}

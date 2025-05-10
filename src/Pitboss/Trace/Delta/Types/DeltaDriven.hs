{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}

module Pitboss.Trace.Delta.Types.DeltaDriven where

class DeltaDriven entity delta | entity -> delta where
  applyDelta :: delta -> entity -> entity
  previewDelta :: delta -> entity -> Maybe entity
  describeDelta :: delta -> entity -> String

-- validateDelta :: delta -> entity -> Either String delta
-- validateDelta d _ = Right d

data TimedDelta d t = TimedDelta {timestamp :: t, delta :: d}

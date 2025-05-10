module Pitboss.Trace.Delta.Round where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Trace.Delta.Types.DeltaDriven
import Pitboss.Trace.Entity.Round hiding (_tick)
import Pitboss.Trace.Entity.Shoe hiding (_tick)
import Pitboss.Trace.Registry.EntityRef

data RoundDelta
  = SetShoeUsed (EntityRef ShoeState)
  | SetRoundNumber Int
  | SetActive Bool
  deriving (Eq, Show, Generic)

instance ToJSON RoundDelta

instance FromJSON RoundDelta

instance DeltaDriven RoundState RoundDelta where
  applyDelta d rs = case d of
    SetShoeUsed ref -> rs {_shoeUsed = ref}
    SetRoundNumber n -> rs {_roundNumber = n}
    SetActive b -> rs {_isActive = b}

  describeDelta :: RoundDelta -> entity -> String
  describeDelta d _ = case d of
    SetShoeUsed ref -> "Set shoe used: " ++ show ref
    SetRoundNumber n -> "Set round number to " ++ show n
    SetActive b -> "Set active status to " ++ show b

  previewDelta d rs = Just (applyDelta d rs)

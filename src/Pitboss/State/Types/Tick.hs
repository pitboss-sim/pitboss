module Pitboss.State.Types.Tick where

import Data.Hashable (Hashable (..))
import Data.Word (Word64)

import Data.Aeson (FromJSON (..), FromJSONKey, ToJSON (..), ToJSONKey)
import GHC.Generics (Generic)

newtype Tick = Tick Word64
    deriving (Eq, Ord, Show, Hashable, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

data Clocked a = Clocked' Tick a
    deriving (Eq, Ord, Show, Generic)

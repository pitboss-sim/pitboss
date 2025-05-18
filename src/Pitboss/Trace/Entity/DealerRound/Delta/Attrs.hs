{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.DealerRound.Delta.Attrs where

import Data.Aeson
import GHC.Generics
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.DealerRound.Types

data DealerRoundEntityAttrsDelta
    = SetDealerRoundEntityNumber Int
    | SetActive Bool
    deriving (Eq, Show, Generic)

instance ToJSON DealerRoundEntityAttrsDelta
instance FromJSON DealerRoundEntityAttrsDelta

instance Incremental DealerRoundEntityAttrsDelta where
    type Target DealerRoundEntityAttrsDelta = DealerRoundEntityAttrs

    applyDelta :: DealerRoundEntityAttrsDelta -> DealerRoundEntityAttrs -> DealerRoundEntityAttrs
    applyDelta delta state = case delta of
        SetDealerRoundEntityNumber n -> state{_dealerRoundEntityAttrsNumber = n}
        SetActive b -> state{_dealerRoundEntityAttrsIsActive = b}

    previewDelta :: DealerRoundEntityAttrsDelta -> DealerRoundEntityAttrs -> Maybe DealerRoundEntityAttrs
    previewDelta delta entity = Just $ applyDelta delta entity

    describeDelta :: DealerRoundEntityAttrsDelta -> DealerRoundEntityAttrs -> String
    describeDelta delta _ = case delta of
        SetDealerRoundEntityNumber n -> "Set round number to " ++ show n
        SetActive b -> "Set active status to " ++ show b

instance Reversible DealerRoundEntityAttrsDelta where
    invert = \case
        SetDealerRoundEntityNumber _ -> Left NotInvertible
        SetActive b -> Right (SetActive (not b))

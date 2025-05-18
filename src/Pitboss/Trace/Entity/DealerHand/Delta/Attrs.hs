{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.DealerHand.Delta.Attrs where

import Data.Aeson
import GHC.Generics
import Pitboss.Blackjack.Card
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.DealerHand.Types

data DealerHandEntityAttrsDelta
    = AddCard Card
    | RemoveCard Card
    | ReplaceCards [Card] [Card] -- old, new
    deriving (Eq, Show, Generic)

instance ToJSON DealerHandEntityAttrsDelta
instance FromJSON DealerHandEntityAttrsDelta

instance Incremental DealerHandEntityAttrsDelta where
    type Target DealerHandEntityAttrsDelta = DealerHandEntityAttrs

    applyDelta = \case
        AddCard c -> \s -> s{_dealerHandEntityAttrsHandCards = c : _dealerHandEntityAttrsHandCards s}
        RemoveCard c -> \s -> s{_dealerHandEntityAttrsHandCards = filter (/= c) (_dealerHandEntityAttrsHandCards s)}
        ReplaceCards _ new -> \s -> s{_dealerHandEntityAttrsHandCards = new}

    previewDelta d s = Just $ applyDelta d s

    describeDelta d _ = case d of
        AddCard c -> "Added card: " ++ show c
        RemoveCard c -> "Removed card: " ++ show c
        ReplaceCards old new -> "Replaced cards: " ++ show old ++ " -> " ++ show new

instance Reversible DealerHandEntityAttrsDelta where
    invert = \case
        AddCard c -> Right (RemoveCard c)
        RemoveCard c -> Right (AddCard c)
        ReplaceCards old new -> Right (ReplaceCards new old)

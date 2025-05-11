{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.DealerHand.Delta.Attrs where

import Data.Aeson
import GHC.Generics
import Pitboss.Blackjack.Card
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.DealerHand

data DealerHandEntityAttrsDelta
    = AddCard Card
    | RemoveCard Card
    | ReplaceCards [Card] [Card]
    deriving (Eq, Show, Generic)

instance ToJSON DealerHandEntityAttrsDelta

instance FromJSON DealerHandEntityAttrsDelta

instance Incremental DealerHandEntityAttrsDelta where
    type Target DealerHandEntityAttrsDelta = DealerHandEntityAttrs

    applyDelta delta state = case delta of
        AddCard c -> state{_dealerHandEntityAttrsHandCards = c : _dealerHandEntityAttrsHandCards state}
        RemoveCard c -> state{_dealerHandEntityAttrsHandCards = filter (/= c) (_dealerHandEntityAttrsHandCards state)}
        ReplaceCards _ new -> state{_dealerHandEntityAttrsHandCards = new}

    previewDelta delta state = Just $ applyDelta delta state

    describeDelta delta _ = case delta of
        AddCard c -> "Added card: " ++ show c
        RemoveCard c -> "Removed card: " ++ show c
        ReplaceCards old new -> "Replaced cards: " ++ show old ++ " → " ++ show new

instance Reversible DealerHandEntityAttrsDelta where
    invert = \case
        AddCard c -> Right (RemoveCard c)
        RemoveCard c -> Right (AddCard c)
        ReplaceCards old new -> Right (ReplaceCards new old)

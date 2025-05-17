{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.DealerHand.Delta.Attrs where

import Data.Aeson
import GHC.Generics
import Pitboss.Blackjack.Card
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.DealerHand

data DealerHandStateDelta
  = AddCard Card
  | RemoveCard Card
  | ReplaceCards [Card] [Card]
  deriving (Eq, Show, Generic)

instance ToJSON DealerHandStateDelta

instance FromJSON DealerHandStateDelta

instance Incremental DealerHandStateDelta where
  type Entity DealerHandStateDelta = DealerHandState

  applyDelta delta state = case delta of
    AddCard c -> state {_dealerHandStateHandCards = c : _dealerHandStateHandCards state}
    RemoveCard c -> state {_dealerHandStateHandCards = filter (/= c) (_dealerHandStateHandCards state)}
    ReplaceCards _ new -> state {_dealerHandStateHandCards = new}

  previewDelta delta state = Just $ applyDelta delta state

  describeDelta delta _ = case delta of
    AddCard c -> "Added card: " ++ show c
    RemoveCard c -> "Removed card: " ++ show c
    ReplaceCards old new -> "Replaced cards: " ++ show old ++ " → " ++ show new

instance Reversible DealerHandStateDelta where
  invert = \case
    AddCard c -> Right (RemoveCard c)
    RemoveCard c -> Right (AddCard c)
    ReplaceCards old new -> Right (ReplaceCards new old)

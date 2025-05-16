{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Delta.DealerHand.State where

import Data.Aeson
import GHC.Generics
import Pitboss.Blackjack.Card
import Pitboss.Trace.Delta.Capabilities.Incremental
import Pitboss.Trace.Delta.Capabilities.Reversible
import Pitboss.Trace.Entity.DealerHand

data DealerHandStateDelta
  = AddCard Card
  | RemoveCard Card
  | ReplaceCards [Card] [Card]
  | ReplaceDealerHandIndex Int Int
  | ReplaceSplitDepth Int Int
  deriving (Eq, Show, Generic)

instance ToJSON DealerHandStateDelta

instance FromJSON DealerHandStateDelta

instance Incremental DealerHandStateDelta where
  type Entity DealerHandStateDelta = DealerHandState

  applyDelta delta state = case delta of
    AddCard c -> state {_handCards = c : _handCards state}
    RemoveCard c -> state {_handCards = filter (/= c) (_handCards state)}
    ReplaceCards _ new -> state {_handCards = new}
    ReplaceDealerHandIndex _ new -> state {_handIx = new}
    ReplaceSplitDepth _ new -> state {_splitDepth = new}

  previewDelta delta state = case delta of
    ReplaceDealerHandIndex old _ ->
      if old == _handIx state
        then Just (applyDelta delta state)
        else Nothing
    ReplaceSplitDepth old _ ->
      if old == _splitDepth state
        then Just (applyDelta delta state)
        else Nothing
    _ -> Just (applyDelta delta state)

  describeDelta delta _ = case delta of
    AddCard c -> "Added card: " ++ show c
    RemoveCard c -> "Removed card: " ++ show c
    ReplaceCards old new -> "Replaced cards: " ++ show old ++ " → " ++ show new
    ReplaceDealerHandIndex old new -> "Changed hand index: " ++ show old ++ " → " ++ show new
    ReplaceSplitDepth old new -> "Changed split depth: " ++ show old ++ " → " ++ show new

instance Reversible DealerHandStateDelta where
  invert = \case
    AddCard c -> Right (RemoveCard c)
    RemoveCard c -> Right (AddCard c)
    ReplaceCards old new -> Right (ReplaceCards new old)
    ReplaceDealerHandIndex old new -> Right (ReplaceDealerHandIndex new old)
    ReplaceSplitDepth old new -> Right (ReplaceSplitDepth new old)

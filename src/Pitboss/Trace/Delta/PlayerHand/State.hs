{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Delta.PlayerHand.State where

import Data.Aeson
import GHC.Generics
import Pitboss.Blackjack.Card
import Pitboss.Trace.Delta.Capabilities.Incremental
import Pitboss.Trace.Delta.Capabilities.Reversible
import Pitboss.Trace.Entity.PlayerHand

data PlayerHandStateDelta
  = AddCard Card
  | RemoveCard Card
  | ReplaceCards [Card] [Card]
  | ReplacePlayerHandIndex Int Int
  | ReplaceSplitDepth Int Int
  deriving (Eq, Show, Generic)

instance ToJSON PlayerHandStateDelta

instance FromJSON PlayerHandStateDelta

instance Incremental PlayerHandStateDelta where
  type Entity PlayerHandStateDelta = PlayerHandState

  applyDelta delta state = case delta of
    AddCard c -> state {_handCards = _handCards state ++ [c]}
    RemoveCard c -> state {_handCards = filter (/= c) (_handCards state)}
    ReplaceCards _ new -> state {_handCards = new}
    ReplacePlayerHandIndex _ new -> state {_handIx = new}
    ReplaceSplitDepth _ new -> state {_splitDepth = new}

  previewDelta delta state = case delta of
    ReplacePlayerHandIndex old _ ->
      if old == _handIx state
        then Just $ applyDelta delta state
        else Nothing
    ReplaceSplitDepth old _ ->
      if old == _splitDepth state
        then Just $ applyDelta delta state
        else Nothing
    _ -> Just $ applyDelta delta state

  describeDelta delta _ = case delta of
    AddCard c -> "Added card: " ++ show c
    RemoveCard c -> "Removed card: " ++ show c
    ReplaceCards old new -> "Replaced cards: " ++ show old ++ " → " ++ show new
    ReplacePlayerHandIndex old new -> "Changed hand index: " ++ show old ++ " → " ++ show new
    ReplaceSplitDepth old new -> "Changed split depth: " ++ show old ++ " → " ++ show new

instance Reversible PlayerHandStateDelta where
  invert = \case
    AddCard c -> Right (RemoveCard c)
    RemoveCard c -> Right (AddCard c)
    ReplaceCards old new -> Right (ReplaceCards new old)
    ReplacePlayerHandIndex old new -> Right (ReplacePlayerHandIndex new old)
    ReplaceSplitDepth old new -> Right (ReplaceSplitDepth new old)

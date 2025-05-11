{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.PlayerHand.Delta.Attrs where

import Data.Aeson
import GHC.Generics
import Pitboss.Blackjack.Card
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.PlayerHand

data PlayerHandEntityAttrsDelta
    = AddCard Card
    | RemoveCard Card
    | ReplaceCards [Card] [Card]
    | ReplacePlayerHandIndex Int Int
    | ReplaceSplitDepth Int Int
    deriving (Eq, Show, Generic)

instance ToJSON PlayerHandEntityAttrsDelta
instance FromJSON PlayerHandEntityAttrsDelta

instance Incremental PlayerHandEntityAttrsDelta where
    type Target PlayerHandEntityAttrsDelta = PlayerHandEntityAttrs

    applyDelta delta state = case delta of
        AddCard c -> state{_playerHandEntityAttrsHandCards = _playerHandEntityAttrsHandCards state ++ [c]}
        RemoveCard c -> state{_playerHandEntityAttrsHandCards = filter (/= c) (_playerHandEntityAttrsHandCards state)}
        ReplaceCards _ new -> state{_playerHandEntityAttrsHandCards = new}
        ReplacePlayerHandIndex _ new -> state{_playerHandEntityAttrsHandIx = new}
        ReplaceSplitDepth _ new -> state{_playerHandEntityAttrsSplitDepth = new}

    previewDelta delta state = case delta of
        ReplacePlayerHandIndex old _ | old == _playerHandEntityAttrsHandIx state -> Just (applyDelta delta state)
        ReplaceSplitDepth old _ | old == _playerHandEntityAttrsSplitDepth state -> Just (applyDelta delta state)
        _ -> Just (applyDelta delta state)

    describeDelta delta _ = case delta of
        AddCard c -> "Added card: " ++ show c
        RemoveCard c -> "Removed card: " ++ show c
        ReplaceCards old new -> "Replaced cards: " ++ show old ++ " -> " ++ show new
        ReplacePlayerHandIndex old new -> "Changed hand index: " ++ show old ++ " -> " ++ show new
        ReplaceSplitDepth old new -> "Changed split depth: " ++ show old ++ " -> " ++ show new

instance Reversible PlayerHandEntityAttrsDelta where
    invert = \case
        AddCard c -> Right (RemoveCard c)
        RemoveCard c -> Right (AddCard c)
        ReplaceCards old new -> Right (ReplaceCards new old)
        ReplacePlayerHandIndex old new -> Right (ReplacePlayerHandIndex new old)
        ReplaceSplitDepth old new -> Right (ReplaceSplitDepth new old)

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Delta.Table.State where

import Data.Aeson
import GHC.Generics
import Pitboss.Blackjack.Chips
import Pitboss.Trace.Delta.Capabilities.Incremental
import Pitboss.Trace.Delta.Capabilities.Reversible
import Pitboss.Trace.Entity.Table
import Pitboss.Trace.Types.EntityRef
import Pitboss.Trace.Types.Identifier

data TableStateDelta
  = SetTableName String String
  | SetMinBet Chips Chips
  | SetOffering (EntityRef OfferingId) (EntityRef OfferingId)
  | StartRound (Maybe (EntityRef DealerRoundId)) (EntityRef DealerRoundId)
  | EndRound (EntityRef DealerRoundId)
  deriving (Eq, Show, Generic)

instance ToJSON TableStateDelta

instance FromJSON TableStateDelta

instance Incremental TableStateDelta where
  type Entity TableStateDelta = TableState

  applyDelta delta s = case delta of
    SetTableName _ new -> s {_tableName = new}
    SetMinBet _ new -> s {_minBet = new}
    SetOffering _ new -> s {_offeringUsed = new}
    StartRound _ new -> s {_currentRound = Just new}
    EndRound _ -> s {_currentRound = Nothing}

  previewDelta delta entity = case delta of
    SetTableName old _ ->
      if old == _tableName entity
        then Just $ applyDelta delta entity
        else Nothing
    SetMinBet old _ ->
      if old == _minBet entity
        then Just $ applyDelta delta entity
        else Nothing
    SetOffering old _ ->
      if old == _offeringUsed entity
        then Just $ applyDelta delta entity
        else Nothing
    StartRound prev _ ->
      if _currentRound entity == prev
        then Just $ applyDelta delta entity
        else Nothing
    EndRound old ->
      if _currentRound entity == Just old
        then Just $ applyDelta delta entity
        else Nothing

  describeDelta delta _ = case delta of
    SetTableName old new ->
      "Changed table name: " ++ old ++ " → " ++ new
    SetMinBet old new ->
      "Changed minimum bet: " ++ show old ++ " → " ++ show new
    SetOffering old new ->
      "Changed offering: " ++ show old ++ " → " ++ show new
    StartRound prev new ->
      "Started round: " ++ show new ++ maybe "" (\p -> " (previous: " ++ show p ++ ")") prev
    EndRound old ->
      "Ended round: " ++ show old

instance Reversible TableStateDelta where
  invert = \case
    SetTableName old new -> Right (SetTableName new old)
    SetMinBet old new -> Right (SetMinBet new old)
    SetOffering old new -> Right (SetOffering new old)
    StartRound _ new -> Right (EndRound new)
    EndRound old -> Right (StartRound (Just old) old)

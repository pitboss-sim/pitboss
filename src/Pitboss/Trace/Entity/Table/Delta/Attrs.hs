{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.Trace.Entity.Table.Delta.Attrs where

import Data.Aeson
import GHC.Generics
import Pitboss.Blackjack.Chips
import Pitboss.Trace.Entity.Capabilities
import Pitboss.Trace.Entity.Table
import Pitboss.Trace.Types.EntityRef
import Pitboss.Trace.Types.Identifier

data TableEntityAttrsDelta
    = SetTableName String String
    | SetMinBet Chips Chips
    | SetOffering (EntityRef OfferingEntityId) (EntityRef OfferingEntityId)
    | StartRound (Maybe (EntityRef DealerRoundEntityId)) (EntityRef DealerRoundEntityId)
    | EndRound (EntityRef DealerRoundEntityId)
    deriving (Eq, Show, Generic)

instance ToJSON TableEntityAttrsDelta

instance FromJSON TableEntityAttrsDelta

instance Incremental TableEntityAttrsDelta where
    type Target TableEntityAttrsDelta = TableEntityAttrs

    applyDelta delta s = case delta of
        SetTableName _ new -> s{_tableEntityAttrsName = new}
        SetMinBet _ new -> s{_tableEntityAttrsMinBet = new}
        SetOffering _ new -> s{_tableEntityAttrsOfferingUsed = new}
        StartRound _ new -> s{_tableEntityAttrsCurrentRound = Just new}
        EndRound _ -> s{_tableEntityAttrsCurrentRound = Nothing}

    previewDelta delta entity = case delta of
        SetTableName old _ ->
            if old == _tableEntityAttrsName entity
                then Just $ applyDelta delta entity
                else Nothing
        SetMinBet old _ ->
            if old == _tableEntityAttrsMinBet entity
                then Just $ applyDelta delta entity
                else Nothing
        SetOffering old _ ->
            if old == _tableEntityAttrsOfferingUsed entity
                then Just $ applyDelta delta entity
                else Nothing
        StartRound prev _ ->
            if _tableEntityAttrsCurrentRound entity == prev
                then Just $ applyDelta delta entity
                else Nothing
        EndRound old ->
            if _tableEntityAttrsCurrentRound entity == Just old
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

instance Reversible TableEntityAttrsDelta where
    invert = \case
        SetTableName old new -> Right (SetTableName new old)
        SetMinBet old new -> Right (SetMinBet new old)
        SetOffering old new -> Right (SetOffering new old)
        StartRound _ new -> Right (EndRound new)
        EndRound old -> Right (StartRound (Just old) old)

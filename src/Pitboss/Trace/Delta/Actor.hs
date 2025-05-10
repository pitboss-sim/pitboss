{-# LANGUAGE LambdaCase #-}

module Pitboss.Trace.Delta.Actor where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Pitboss.Trace.Delta.Types.DeltaDriven
import Pitboss.Trace.Entity.Actor
import Pitboss.Trace.Entity.Table
import Pitboss.Trace.Registry.EntityRef

data ActorDelta
  = RenameActor String
  | AssignTable (EntityRef TableState)
  | UnassignTable
  deriving (Eq, Show, Generic)

instance ToJSON ActorDelta

instance FromJSON ActorDelta

instance DeltaDriven ActorState ActorDelta where
  applyDelta delta = \case
    PlayerActorState t p -> case delta of
      RenameActor name -> PlayerActorState t (p {_playerName = name})
      _ -> PlayerActorState t p
    DealerActorState t d -> case delta of
      RenameActor name -> DealerActorState t (d {_dealerName = name})
      AssignTable tid -> DealerActorState t (d {_assignedTable = Just tid})
      UnassignTable -> DealerActorState t (d {_assignedTable = Nothing})

  describeDelta :: ActorDelta -> entity -> String
  describeDelta d _ = case d of
    RenameActor name -> "Renamed actor to " ++ name
    AssignTable tid -> "Assigned to table " ++ show tid
    UnassignTable -> "Unassigned from table"

  previewDelta d s = Just (applyDelta d s)

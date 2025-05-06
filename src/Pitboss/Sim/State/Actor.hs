-- src/Pitboss/Sim/State/Actor.hs

data PlayerActor = PlayerActor
  { playerFSM :: PlayerFSM,
    preferredTables :: [TableId]
  }

data DealerActor = DealerActor
  { assignedTable :: Maybe TableId
  }

data ActorState
  = Player PlayerActor
  | Dealer DealerActor

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Pitboss.Mechanics.Dealer.Types where

import Data.Aeson
import GHC.Generics (Generic)
import Pitboss.Mechanics.Player.Types
import Pitboss.Mechanics.Types.Transitionable
import Pitboss.Trace.Timeline.Identifier (RoundId, TableId)

data DealerPhase
  = AwaitingRound
  | Dealing
  | Acting
  | Done
  deriving (Eq, Show, Generic)

data DealerFSM (p :: DealerPhase) where
  AwaitingRoundFSM :: DealerFSM 'AwaitingRound
  DealingFSM :: TableId -> RoundId -> DealerFSM 'Dealing
  ActingFSM :: TableId -> RoundId -> DealerFSM 'Acting
  DoneFSM :: DealerFSM 'Done

deriving instance Show (DealerFSM p)

deriving instance Eq (DealerFSM p)

data SomeDealerFSM = forall p. SomeDealerFSM (DealerFSM p)

instance Show SomeDealerFSM where
  show (SomeDealerFSM fsm) = "SomeDealerFSM (" ++ show fsm ++ ")"

instance ToJSON SomeDealerFSM where
  toJSON (SomeDealerFSM fsm) = case fsm of
    AwaitingRoundFSM ->
      object ["tag" .= String "AwaitingRound"]
    DealingFSM tableId roundId ->
      object ["tag" .= String "Dealing", "tableId" .= tableId, "roundId" .= roundId]
    ActingFSM tableId roundId ->
      object ["tag" .= String "Acting", "tableId" .= tableId, "roundId" .= roundId]
    DoneFSM ->
      object ["tag" .= String "Done"]

instance FromJSON SomeDealerFSM where
  parseJSON = withObject "SomeDealerFSM" $ \obj -> do
    tag <- obj .: "tag"
    case tag :: String of
      "AwaitingRound" ->
        pure $ SomeDealerFSM AwaitingRoundFSM
      "Dealing" ->
        SomeDealerFSM <$> (DealingFSM <$> obj .: "tableId" <*> obj .: "roundId")
      "Acting" ->
        SomeDealerFSM <$> (ActingFSM <$> obj .: "tableId" <*> obj .: "roundId")
      "Done" ->
        pure $ SomeDealerFSM DoneFSM
      other ->
        fail $ "Unknown tag for SomeDealerFSM: " ++ other

instance Transitionable (DealerFSM p) where
  transitionType = \case
    AwaitingRoundFSM -> AwaitInput
    DealingFSM _ _ -> AutoAdvance
    ActingFSM _ _ -> AwaitInput
    DoneFSM -> TerminalPhase

data DealerHandFSM
  = DealerActing
  | DealerDone HandResolution
  deriving (Eq, Show)

instance Transitionable DealerHandFSM where
  transitionType = \case
    DealerActing -> AwaitInput
    DealerDone _ -> TerminalPhase

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Pitboss.FSM.PlayerHand (
    module Pitboss.FSM.PlayerHand.FSM,
    module Pitboss.FSM.PlayerHand.Phase,
    module Pitboss.FSM.PlayerHand.Transition,
    SomePlayerHandFSM (..),
    isHandTerminal,
    resolutionImpact,
)
where

import Data.Aeson.Types
import Data.Text (Text)
import Data.Text qualified as T
import Pitboss.FSM.PlayerHand.FSM
import Pitboss.FSM.PlayerHand.Phase
import Pitboss.FSM.PlayerHand.Transition
import Pitboss.FSM.Types.Transitionable

data SomePlayerHandFSM = forall p h d s. SomePlayerHandFSM (PlayerHandFSM p h d s)

instance Show SomePlayerHandFSM where
    show (SomePlayerHandFSM fsm) = "SomePlayerHandFSM (" ++ show fsm ++ ")"

instance Eq SomePlayerHandFSM where
    (SomePlayerHandFSM f1) == (SomePlayerHandFSM f2) = case (f1, f2) of
        (AbandonedFSM r1, AbandonedFSM r2) -> r1 == r2
        (BlackjackFSM, BlackjackFSM) -> True
        (DecisionFSM, DecisionFSM) -> True
        (HittingFSM, HittingFSM) -> True
        (OneCardDrawFSM r1, OneCardDrawFSM r2) -> r1 == r2
        (ResolvedFSM r1, ResolvedFSM r2) -> r1 == r2
        _ -> False

instance ToJSON SomePlayerHandFSM where
    toJSON (SomePlayerHandFSM fsm) = case fsm of
        AbandonedFSM reason ->
            object ["tag" .= String "Abandoned", "reason" .= reason]
        BlackjackFSM ->
            object ["tag" .= String "NaturalBlackjack"]
        DecisionFSM ->
            object ["tag" .= String "Decision"]
        HittingFSM ->
            object ["tag" .= String "Hitting"]
        OneCardDrawFSM reason ->
            object ["tag" .= String "OneCardDraw", "reason" .= reason]
        ResolvedFSM resolution ->
            object ["tag" .= String "Resolved", "resolution" .= resolution]

instance FromJSON SomePlayerHandFSM where
    parseJSON = withObject "SomePlayerHandFSM" $ \obj -> do
        tag <- obj .: "tag"
        case tag :: Text of
            "Abandoned" -> SomePlayerHandFSM . AbandonedFSM <$> obj .: "reason"
            "NaturalBlackjack" -> pure $ SomePlayerHandFSM BlackjackFSM
            "Decision" -> pure $ SomePlayerHandFSM DecisionFSM
            "Hitting" -> pure $ SomePlayerHandFSM HittingFSM
            "OneCardDraw" -> SomePlayerHandFSM . OneCardDrawFSM <$> obj .: "reason"
            "Resolved" -> SomePlayerHandFSM . ResolvedFSM <$> obj .: "resolution"
            _ -> fail $ "Unknown tag for SomePlayerHandFSM: " ++ T.unpack tag

isHandTerminal :: SomePlayerHandFSM -> Bool
isHandTerminal (SomePlayerHandFSM fsm) =
    case transitionType fsm of
        TerminalPhase -> True
        _ -> False

resolutionImpact :: PlayerHandResolution -> Maybe BankrollImpact
resolutionImpact = \case
    Surrendered -> Just Refund
    Push -> Just Refund
    Bust -> Just Loss
    DealerBlackjack -> Just Loss
    Void i -> Just i
    _ -> Nothing

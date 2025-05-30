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
        (PHAbandonedFSM r1, PHAbandonedFSM r2) -> r1 == r2
        (PHBlackjackFSM, PHBlackjackFSM) -> True
        (PHDecisionFSM, PHDecisionFSM) -> True
        (PHHittingFSM, PHHittingFSM) -> True
        (PHOneCardDrawFSM r1, PHOneCardDrawFSM r2) -> r1 == r2
        (PHResolvedFSM r1, PHResolvedFSM r2) -> r1 == r2
        _ -> False

instance ToJSON SomePlayerHandFSM where
    toJSON (SomePlayerHandFSM fsm) = case fsm of
        PHAbandonedFSM reason ->
            object ["tag" .= String "Abandoned", "reason" .= reason]
        PHBlackjackFSM ->
            object ["tag" .= String "NaturalBlackjack"]
        PHDecisionFSM ->
            object ["tag" .= String "Decision"]
        PHHittingFSM ->
            object ["tag" .= String "Hitting"]
        PHOneCardDrawFSM reason ->
            object ["tag" .= String "OneCardDraw", "reason" .= reason]
        PHResolvedFSM resolution ->
            object ["tag" .= String "Resolved", "resolution" .= resolution]

instance FromJSON SomePlayerHandFSM where
    parseJSON = withObject "SomePlayerHandFSM" $ \obj -> do
        tag <- obj .: "tag"
        case tag :: Text of
            "Abandoned" -> SomePlayerHandFSM . PHAbandonedFSM <$> obj .: "reason"
            "NaturalBlackjack" -> pure $ SomePlayerHandFSM PHBlackjackFSM
            "Decision" -> pure $ SomePlayerHandFSM PHDecisionFSM
            "Hitting" -> pure $ SomePlayerHandFSM PHHittingFSM
            "OneCardDraw" -> SomePlayerHandFSM . PHOneCardDrawFSM <$> obj .: "reason"
            "Resolved" -> SomePlayerHandFSM . PHResolvedFSM <$> obj .: "resolution"
            _ -> fail $ "Unknown tag for SomePlayerHandFSM: " ++ T.unpack tag

isHandTerminal :: SomePlayerHandFSM -> Bool
isHandTerminal (SomePlayerHandFSM fsm) =
    case transitionType fsm of
        TerminalPhase -> True
        _ -> False

resolutionImpact :: PlayerHandResolution -> Maybe BankrollImpact
resolutionImpact = \case
    PHSurrendered -> Just Refund
    PHPush -> Just Refund
    PHBust -> Just Loss
    PHDealerBlackjack -> Just Loss
    PHVoid i -> Just i
    _ -> Nothing

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Pitboss.FSM.PlayerHand.Phase where

import Data.Aeson.Types
import Data.Text
import Data.Text qualified as T
import GHC.Generics
import Pitboss.Blackjack hiding (NaturalBlackjack, HandPhase)
import Pitboss.FSM.Types

data HandPhase
    = PHAbandoned AbandonedReason
    | PHNaturalBlackjack
    | PHDecision
    | PHHitting
    | PHOneCardDraw OneCardDrawReason
    | PHResolved PlayerHandResolution
    deriving (Eq, Show, Generic)

data AbandonedReason
    = PHSurrender Surrender
    | PHInsurance InsuranceOutcome
    deriving (Eq, Show, Generic)

data OneCardDrawReason = PHDouble | PHSplitAce
    deriving (Eq, Show, Generic)

data PlayerHandResolution
    = PHSurrendered
    | PHBlackjack
    | PHStand
    | PHBust
    | PHPush
    | PHSplitNonAces
    | PHSplitAces
    | PHDealerBlackjack
    | PHVoid BankrollImpact
    deriving (Eq, Show, Generic)

data BankrollImpact = Loss | Refund
    deriving (Eq, Show, Generic)

instance ToJSON HandPhase where
    toJSON = \case
        PHAbandoned r -> object ["tag" .= String "Abandoned", "reason" .= r]
        PHNaturalBlackjack -> object ["tag" .= String "NaturalBlackjack"]
        PHDecision -> object ["tag" .= String "Decision"]
        PHHitting -> object ["tag" .= String "Hitting"]
        PHOneCardDraw reason -> object ["tag" .= String "OneCardDraw", "reason" .= reason]
        PHResolved res -> object ["tag" .= String "Resolved", "resolution" .= res]

instance FromJSON HandPhase where
    parseJSON = withObject "HandPhase" $ \obj -> do
        tag <- obj .: "tag"
        case tag :: Text of
            "Abandoned" -> PHAbandoned <$> obj .: "reason"
            "NaturalBlackjack" -> pure PHNaturalBlackjack
            "Decision" -> pure PHDecision
            "Hitting" -> pure PHHitting
            "OneCardDraw" -> PHOneCardDraw <$> obj .: "reason"
            "Resolved" -> PHResolved <$> obj .: "resolution"
            _ -> fail $ "Unknown HandPhase tag: " ++ T.unpack tag

instance ToJSON AbandonedReason
instance FromJSON AbandonedReason

instance ToJSON OneCardDrawReason
instance FromJSON OneCardDrawReason

instance ToJSON BankrollImpact
instance FromJSON BankrollImpact

instance ToJSON PlayerHandResolution
instance FromJSON PlayerHandResolution

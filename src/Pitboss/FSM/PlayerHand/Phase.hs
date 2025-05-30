{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Pitboss.FSM.PlayerHand.Phase where

import Data.Aeson.Types
import Data.Text
import Data.Text qualified as T
import GHC.Generics
import Pitboss.Blackjack.Offering.RuleSet
import Pitboss.FSM.Types

data HandPhase
    = Abandoned AbandonedReason
    | NaturalBlackjack
    | Decision
    | Hitting
    | OneCardDraw OneCardDrawReason
    | Resolved PlayerHandResolution
    deriving (Eq, Show, Generic)

data AbandonedReason
    = Surrender Surrender
    | Insurance InsuranceOutcome
    deriving (Eq, Show, Generic)

data OneCardDrawReason = Double | SplitAce
    deriving (Eq, Show, Generic)

data PlayerHandResolution
    = Surrendered
    | Blackjack
    | Stand
    | Bust
    | Push
    | SplitNonAces
    | SplitAces
    | DealerBlackjack
    | Void BankrollImpact
    deriving (Eq, Show, Generic)

data BankrollImpact = Loss | Refund
    deriving (Eq, Show, Generic)

instance ToJSON HandPhase where
    toJSON = \case
        Abandoned r -> object ["tag" .= String "Abandoned", "reason" .= r]
        NaturalBlackjack -> object ["tag" .= String "NaturalBlackjack"]
        Decision -> object ["tag" .= String "Decision"]
        Hitting -> object ["tag" .= String "Hitting"]
        OneCardDraw reason -> object ["tag" .= String "OneCardDraw", "reason" .= reason]
        Resolved res -> object ["tag" .= String "Resolved", "resolution" .= res]

instance FromJSON HandPhase where
    parseJSON = withObject "HandPhase" $ \obj -> do
        tag <- obj .: "tag"
        case tag :: Text of
            "Abandoned" -> Abandoned <$> obj .: "reason"
            "NaturalBlackjack" -> pure NaturalBlackjack
            "Decision" -> pure Decision
            "Hitting" -> pure Hitting
            "OneCardDraw" -> OneCardDraw <$> obj .: "reason"
            "Resolved" -> Resolved <$> obj .: "resolution"
            _ -> fail $ "Unknown HandPhase tag: " ++ T.unpack tag

instance ToJSON AbandonedReason
instance FromJSON AbandonedReason

instance ToJSON OneCardDrawReason
instance FromJSON OneCardDrawReason

instance ToJSON BankrollImpact
instance FromJSON BankrollImpact

instance ToJSON PlayerHandResolution
instance FromJSON PlayerHandResolution

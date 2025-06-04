{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Pitboss.FSM.Player.Hand where

import Data.Aeson.Types
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Pitboss.Blackjack hiding (HandPhase)
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

data SomePlayerHandFSM = forall p h d s. SomePlayerHandFSM (PlayerHandFSM p h d s)

data PlayerHandFSM (p :: HandPhase) (h :: OHit) (d :: ODbl) (s :: OSpl) where
    PHAbandonedFSM :: AbandonedReason -> PlayerHandFSM ('PHAbandoned reason) 'NoHit 'NoDbl 'NoSpl
    PHBlackjackFSM :: PlayerHandFSM 'PHNaturalBlackjack 'NoHit 'NoDbl 'NoSpl
    PHDecisionFSM :: PlayerHandFSM 'PHDecision h d s
    PHHittingFSM :: PlayerHandFSM 'PHHitting h d s
    PHOneCardDrawFSM :: OneCardDrawReason -> PlayerHandFSM ('PHOneCardDraw reason) 'NoHit 'NoDbl 'NoSpl
    PHResolvedFSM :: PlayerHandResolution -> PlayerHandFSM ('PHResolved res) 'NoHit 'NoDbl 'NoSpl

data OHit = OKHit | NoHit
data ODbl = OKDbl | NoDbl
data OSpl = OKSpl | NoSpl

deriving instance Show (PlayerHandFSM p h d s)
deriving instance Eq (PlayerHandFSM p h d s)

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

resolutionImpact :: PlayerHandResolution -> Maybe BankrollImpact
resolutionImpact = \case
    PHSurrendered -> Just Refund
    PHPush -> Just Refund
    PHBust -> Just Loss
    PHDealerBlackjack -> Just Loss
    PHVoid i -> Just i
    _ -> Nothing

type family ValidPlayerHandTransition (from :: HandPhase) (to :: HandPhase) :: Bool where
    ValidPlayerHandTransition 'PHDecision 'PHHitting = 'True
    ValidPlayerHandTransition 'PHDecision ('PHOneCardDraw reason) = 'True
    ValidPlayerHandTransition 'PHDecision ('PHResolved res) = 'True
    ValidPlayerHandTransition 'PHHitting 'PHHitting = 'True
    ValidPlayerHandTransition 'PHHitting ('PHResolved 'PHBust) = 'True
    ValidPlayerHandTransition 'PHHitting ('PHResolved 'PHStand) = 'True
    ValidPlayerHandTransition ('PHOneCardDraw reason) ('PHResolved res) = 'True
    ValidPlayerHandTransition _ _ = 'False

initialDecisionTyped :: SomeHand -> PlayerHandFSM 'PHDecision 'OKHit 'OKDbl 'OKSpl
initialDecisionTyped _hand = PHDecisionFSM

resolveSurrenderTyped ::
    (ValidPlayerHandTransition 'PHDecision ('PHResolved 'PHSurrendered) ~ 'True) =>
    SomeHand ->
    PlayerHandFSM 'PHDecision h d s ->
    PlayerHandFSM ('PHResolved 'PHSurrendered) 'NoHit 'NoDbl 'NoSpl
resolveSurrenderTyped _hand PHDecisionFSM = PHResolvedFSM PHSurrendered

toOneCardDrawFromDecisionTyped ::
    (ValidPlayerHandTransition 'PHDecision ('PHOneCardDraw reason) ~ 'True) =>
    OneCardDrawReason ->
    SomeHand ->
    PlayerHandFSM 'PHDecision 'OKHit d s ->
    PlayerHandFSM ('PHOneCardDraw reason) 'NoHit 'NoDbl 'NoSpl
toOneCardDrawFromDecisionTyped reason _hand PHDecisionFSM = PHOneCardDrawFSM reason

toHittingTyped ::
    (ValidPlayerHandTransition 'PHDecision 'PHHitting ~ 'True) =>
    SomeHand ->
    PlayerHandFSM 'PHDecision 'OKHit d s ->
    PlayerHandFSM 'PHHitting 'OKHit d s
toHittingTyped _hand PHDecisionFSM = PHHittingFSM

continueHittingTyped ::
    (ValidPlayerHandTransition 'PHHitting 'PHHitting ~ 'True) =>
    SomeHand ->
    PlayerHandFSM 'PHHitting h d s ->
    PlayerHandFSM 'PHHitting h d s
continueHittingTyped _hand PHHittingFSM = PHHittingFSM

resolveStandTyped ::
    (ValidPlayerHandTransition 'PHDecision ('PHResolved 'PHStand) ~ 'True) =>
    SomeHand ->
    PlayerHandFSM 'PHDecision h d s ->
    PlayerHandFSM ('PHResolved 'PHStand) 'NoHit 'NoDbl 'NoSpl
resolveStandTyped _hand PHDecisionFSM = PHResolvedFSM PHStand

resolveBustTyped ::
    (ValidPlayerHandTransition 'PHHitting ('PHResolved 'PHBust) ~ 'True) =>
    SomeHand ->
    PlayerHandFSM 'PHHitting h d s ->
    PlayerHandFSM ('PHResolved 'PHBust) 'NoHit 'NoDbl 'NoSpl
resolveBustTyped (SomeHand hand) PHHittingFSM = case witness hand of
    BustWitness -> PHResolvedFSM PHBust
    _ -> error "resolveBustTyped: hand is not busted"

resolveOneCardDrawTyped ::
    (ValidPlayerHandTransition ('PHOneCardDraw reason) ('PHResolved res) ~ 'True) =>
    SomeHand ->
    PlayerHandResolution ->
    PlayerHandFSM ('PHOneCardDraw reason) 'NoHit 'NoDbl 'NoSpl ->
    PlayerHandFSM ('PHResolved res) 'NoHit 'NoDbl 'NoSpl
resolveOneCardDrawTyped (SomeHand hand) res (PHOneCardDrawFSM _) = case (witness hand, res) of
    (BustWitness, PHBust) -> PHResolvedFSM res
    (BlackjackWitness, PHBlackjack) -> PHResolvedFSM res
    (_, PHStand) | handScore (SomeHand hand) <= 21 -> PHResolvedFSM res
    _ -> error "resolveOneCardDrawTyped: invalid resolution for hand"

resolveSplitTyped ::
    (ValidPlayerHandTransition 'PHDecision ('PHResolved res) ~ 'True) =>
    SomeHand ->
    PlayerHandFSM 'PHDecision h d s ->
    Offering ->
    PlayerHandFSM ('PHResolved res) 'NoHit 'NoDbl 'NoSpl
resolveSplitTyped someHand PHDecisionFSM offering =
    case fromSomeHand someHand of
        SomeLifecycleHand fullHand@(FullLifecycleHand _) ->
            if canSplitHand fullHand 0 offering
                then case extractPairRank someHand of
                    Just Ace -> PHResolvedFSM PHSplitAces
                    Just _ -> PHResolvedFSM PHSplitNonAces
                    Nothing -> error "resolveSplitTyped: not a pair"
                else error "resolveSplitTyped: hand cannot be split"
        _ -> error "resolveSplitTyped: not a full hand"

resolvePushTyped ::
    (ValidPlayerHandTransition from ('PHResolved 'PHPush) ~ 'True) =>
    SomeHand ->
    PlayerHandFSM from h d s ->
    PlayerHandFSM ('PHResolved 'PHPush) 'NoHit 'NoDbl 'NoSpl
resolvePushTyped _hand _ = PHResolvedFSM PHPush

resolveDealerBlackjackTyped ::
    (ValidPlayerHandTransition from ('PHResolved 'PHDealerBlackjack) ~ 'True) =>
    SomeHand ->
    PlayerHandFSM from h d s ->
    PlayerHandFSM ('PHResolved 'PHDealerBlackjack) 'NoHit 'NoDbl 'NoSpl
resolveDealerBlackjackTyped _hand _ = PHResolvedFSM PHDealerBlackjack

resolveVoidTyped ::
    (ValidPlayerHandTransition from ('PHResolved ('PHVoid impact)) ~ 'True) =>
    SomeHand ->
    BankrollImpact ->
    PlayerHandFSM from h d s ->
    PlayerHandFSM ('PHResolved ('PHVoid impact)) 'NoHit 'NoDbl 'NoSpl
resolveVoidTyped _hand impact _ = PHResolvedFSM (PHVoid impact)

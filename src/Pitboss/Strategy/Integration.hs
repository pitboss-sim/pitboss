{-# LANGUAGE LambdaCase #-}

module Pitboss.Strategy.Integration where

import Pitboss.Blackjack.Card (Rank (..))
import Pitboss.Blackjack.Hand (Hand, unHand)
import Pitboss.Blackjack.Hand.Category (HandCategory (..), categorize)
import Pitboss.Blackjack.Hand.Scoring (HandScore (..), scoreHand, canDouble, canSplit)
import Pitboss.Blackjack.Offering.RuleSet (RuleSet(..), SplitHands (..), Surrender (..))
import Pitboss.Strategy.Chart.Types (StrategyChart)
import Pitboss.Strategy.Chart.Interpret (lookupDecision)
import Pitboss.Strategy.Chart.Overlay (overlayStrategy, reduceOverlay)
import Pitboss.Strategy.Types (Decision(..), Move(..), Fallback(..))
import Data.Maybe (fromMaybe)

data StrategyContext = StrategyContext
    { baseChart :: StrategyChart
    , countDeviations :: Maybe StrategyChart
    , ruleAdjustments :: Maybe StrategyChart
    , gameRules :: RuleSet
    , handCount :: Int
    , runningCount :: Maybe Int
    , trueCount :: Maybe Double
    } deriving (Eq, Show)

data StrategyDecision = StrategyDecision
    { primaryMove :: Move
    , fallbackMove :: Maybe Move
    , confidence :: StrategyConfidence
    , reasoning :: String
    , chartSource :: ChartSource
    } deriving (Eq, Show)

data StrategyConfidence
    = HighConfidence
    | MediumConfidence
    | LowConfidence
    deriving (Eq, Show, Ord)

data ChartSource
    = BaseChart
    | CountDeviation
    | RuleAdjustment
    | Fallback
    deriving (Eq, Show)

consultStrategy ::
    StrategyContext ->
    Hand ->
    Rank ->
    Int ->
    StrategyDecision
consultStrategy ctx playerHand dealerUp splitDepth =
    let handCat = categorize (unHand playerHand)
        handScore = scoreHand playerHand
        layeredChart = buildLayeredChart ctx handCat splitDepth
        baseDecision = lookupDecision layeredChart handCat dealerUp
        constrainedDecision = constrainByRules (gameRules ctx) handScore splitDepth baseDecision
        (source, confidence) = determineSource ctx handCat dealerUp
    in case constrainedDecision of
        Just (Always move) -> StrategyDecision
            { primaryMove = move
            , fallbackMove = Nothing
            , confidence = confidence
            , reasoning = explainDecision handCat dealerUp move
            , chartSource = source
            }
        Just (Prefer primary (Else fallback)) -> StrategyDecision
            { primaryMove = primary
            , fallbackMove = Just fallback
            , confidence = confidence
            , reasoning = explainDecision handCat dealerUp primary
            , chartSource = source
            }
        Nothing -> StrategyDecision
            { primaryMove = Hit
            , fallbackMove = Nothing
            , confidence = LowConfidence
            , reasoning = "No strategy found - defaulting to Hit"
            , chartSource = Fallback
            }

buildLayeredChart :: StrategyContext -> HandCategory -> Int -> StrategyChart
buildLayeredChart ctx _ _ =
    let charts = [baseChart ctx]
                ++ maybe [] pure (ruleAdjustments ctx)
                ++ maybe [] pure (countDeviations ctx)
    in reduceOverlay charts

constrainByRules :: RuleSet -> HandScore -> Int -> Maybe Decision -> Maybe Decision
constrainByRules rules handScore splitDepth decision = case decision of
    Just (Always Double) ->
        if canDouble rules (scoreValue handScore)
        then Just (Always Double)
        else Just (Always Hit)

    Just (Prefer Double fallback) ->
        if cwnDouble rules (scoreValue handScore)
        then Just (Prefer Double fallback)
        else Just (Always Hit)

    Just (Always Split) ->
        if canSplitByRules rules splitDepth
        then Just (Always Split)
        else Just (Always Hit)

    Just (Prefer Split fallback) ->
        if canSplitByRules rules splitDepth
        then Just (Prefer Split fallback)
        else case fallback of
            Else move -> Just (Always move)

    Just (Always Surrender) ->
        if surrenderAllowed rules
        then Just (Always Surrender)
        else Just (Always Stand)

    Just (Prefer Surrender fallback) ->
        if surrenderAllowed rules
        then Just (Prefer Surrender fallback)
        else case fallback of
            Else move -> Just (Always move)

    other -> other

scoreValue :: HandScore -> Int
scoreValue = \case
    Natural -> 21
    Hard n -> n
    Soft n -> n
    Busted -> 0

canSplitByRules :: RuleSet -> Int -> Bool
canSplitByRules rules currentDepth =
    case splitHands rules of
        SP2 -> currentDepth < 2
        SP3 -> currentDepth < 3
        SP4 -> currentDepth < 4

surrenderAllowed :: RuleSet -> Bool
surrenderAllowed rules = case surrender rules of
    NoSurrender -> False
    _ -> True

determineSource :: StrategyContext -> HandCategory -> Rank -> (ChartSource, StrategyConfidence)
determineSource ctx handCat dealerUp =
    case countDeviations ctx >>= \chart -> lookupDecision chart handCat dealerUp of
        Just _ -> (CountDeviation, MediumConfidence)
        Nothing ->
            case ruleAdjustments ctx >>= \chart -> lookupDecision chart handCat dealerUp of
                Just _ -> (RuleAdjustment, MediumConfidence)
                Nothing -> (BaseChart, HighConfidence)

explainDecision :: HandCategory -> Rank -> Move -> String
explainDecision handCat dealerUp move =
    let handDesc = case handCat of
            Empty -> "Empty hand"
            Partial r -> "Single " ++ show r
            Blackjack -> "Natural blackjack"
            TwentyOne -> "21"
            Pair r -> "Pair of " ++ show r ++ "s"
            Soft2 r -> "Soft " ++ show (11 + rankValue r)
            SoftN _ -> "Soft hand"
            Hard2 r1 r2 -> "Hard " ++ show (rankValue r1 + rankValue r2)
            HardN _ -> "Hard hand"

        moveDesc = case move of
            Hit -> "hit"
            Stand -> "stand"
            Double -> "double down"
            Split -> "split"
            Surrender -> "surrender"

    in handDesc ++ " vs dealer " ++ show dealerUp ++ ": " ++ moveDesc

rankValue :: Rank -> Int
rankValue = \case
    Two -> 2; Three -> 3; Four -> 4; Five -> 5; Six -> 6
    Seven -> 7; Eight -> 8; Nine -> 9; Ten -> 10
    Jack -> 10; Queen -> 10; King -> 10; Ace -> 1

adjustForCount ::
    StrategyContext ->
    HandCategory ->
    Rank ->
    StrategyDecision ->
    StrategyDecision
adjustForCount ctx handCat dealerUp baseDecision =
    case trueCount ctx of
        Just tc ->
            let adjustment = getCountAdjustment handCat dealerUp tc
            in fromMaybe $ baseDecision id adjustment
        Nothing -> baseDecision

getCountAdjustment :: HandCategory -> Rank -> Double -> Maybe StrategyDecision
getCountAdjustment handCat dealerUp trueCount =
    case (handCat, dealerUp, trueCount >= 3.0) of
        (Hard2 _ _, Ten, True) -> Just $ StrategyDecision Stand Nothing HighConfidence "Insurance favorable at TC +3" CountDeviation
        _ -> Nothing

vegasStrategy :: RuleSet -> StrategyContext
vegasStrategy rules = StrategyContext
    { baseChart = []
    , countDeviations = Nothing
    , ruleAdjustments = Nothing
    , gameRules = rules
    , handCount = 0
    , runningCount = Nothing
    , trueCount = Nothing
    }

withCounting :: StrategyContext -> Int -> Double -> StrategyContext
withCounting ctx running true = ctx
    { runningCount = Just running
    , trueCount = Just true
    }{-# LANGUAGE LambdaCase #-}


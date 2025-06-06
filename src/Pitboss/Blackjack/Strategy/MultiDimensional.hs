{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Pitboss.Blackjack.Strategy.MultiDimensional where

import Data.Aeson.Types
import Data.Map.Strict qualified as Map
import GHC.Generics (Generic)
import Pitboss.Blackjack.Rules hiding (dealerUpcard)
import Pitboss.Blackjack.Types

data StrategyKey = StrategyKey
    { skValueType :: ValueWitness
    , skStructure :: StructureWitness
    , skNumericValue :: Int
    , skDealerRank :: Rank
    , skCanDouble :: Bool
    , skCanSplit :: Bool
    , skCanSurrender :: Bool
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

witnessKey :: HandWitness -> Rank -> [ActionWitness] -> StrategyKey
witnessKey witness dealerRank actions =
    StrategyKey
        { skValueType = valueType witness
        , skStructure = structure witness
        , skNumericValue = numericValue witness
        , skDealerRank = dealerRank
        , skCanDouble = CanDoubleWitness `elem` actions
        , skCanSplit = CanSplitWitness `elem` actions
        , skCanSurrender = CanSurrenderWitness `elem` actions
        }

type StrategyTable = Map.Map StrategyKey Move

basicStrategyTable :: StrategyTable
basicStrategyTable =
    Map.fromList $
        concat
            [ pairStrategies
            , softStrategies
            , hardStrategies
            , blackjackStrategies
            ]

pairStrategies :: [(StrategyKey, Move)]
pairStrategies =
    [ (StrategyKey HardWitness (PairWitness Ace) 12 dealerRank canDouble True canSurrender, MSplit)
    | dealerRank <- [Two .. Ace]
    , canDouble <- [True, False]
    , canSurrender <- [True, False]
    ]
        ++ [ (StrategyKey HardWitness (PairWitness Eight) 16 dealerRank canDouble True canSurrender, MSplit)
           | dealerRank <- [Two .. Ace]
           , canDouble <- [True, False]
           , canSurrender <- [True, False]
           ]
        ++ [ (StrategyKey HardWitness (PairWitness Ten) 20 dealerRank canDouble True canSurrender, MStand)
           | dealerRank <- [Two .. Ace]
           , canDouble <- [True, False]
           , canSurrender <- [True, False]
           ]
        ++ [ (StrategyKey HardWitness (PairWitness Five) 10 dealerRank True True canSurrender, MDouble)
           | dealerRank <- [Two .. Nine]
           , canSurrender <- [True, False]
           ]

softStrategies :: [(StrategyKey, Move)]
softStrategies =
    [ ( StrategyKey SoftWitness NonPairWitness total dealerRank canDouble False canSurrender
      , decideSoftStrategy total dealerRank canDouble
      )
    | total <- [13 .. 21]
    , dealerRank <- [Two .. Ace]
    , canDouble <- [True, False]
    , canSurrender <- [True, False]
    ]

hardStrategies :: [(StrategyKey, Move)]
hardStrategies =
    [ ( StrategyKey HardWitness NonPairWitness total dealerRank canDouble False canSurrender
      , decideHardStrategy total dealerRank canDouble canSurrender
      )
    | total <- [4 .. 21]
    , dealerRank <- [Two .. Ace]
    , canDouble <- [True, False]
    , canSurrender <- [True, False]
    ]

blackjackStrategies :: [(StrategyKey, Move)]
blackjackStrategies =
    [ (StrategyKey BlackjackWitness NonPairWitness 21 dealerRank False False False, MStand)
    | dealerRank <- [Two .. Ace]
    ]

basicStrategy :: SomeHand -> Card -> GameRuleSet -> Int -> Move
basicStrategy hand dealerUpcard rules splitCount =
    let contextWitness = computeContextualWitness (handCards hand) rules splitCount
        key = witnessKey contextWitness (rank dealerUpcard) (availableActions contextWitness)
     in case Map.lookup key basicStrategyTable of
            Just move -> move
            Nothing -> fallbackStrategy contextWitness dealerUpcard

fallbackStrategy :: HandWitness -> Card -> Move
fallbackStrategy witness dealerUpcard = case valueType witness of
    BlackjackWitness -> MStand
    BustWitness -> MStand
    _ -> chooseMove witness dealerUpcard

chooseMove :: HandWitness -> Card -> Move
chooseMove witness dealerUpcard =
    case structure witness of
        PairWitness rank
            | CanSplitWitness `elem` availableActions witness ->
                decideSplit rank dealerUpcard
        _ -> case (valueType witness, availableActions witness) of
            (_, actions)
                | CanSurrenderWitness `elem` actions ->
                    decideSurrender (numericValue witness) dealerUpcard
            (SoftWitness, actions)
                | CanDoubleWitness `elem` actions ->
                    decideSoftDouble (numericValue witness) dealerUpcard
            (HardWitness, actions)
                | CanDoubleWitness `elem` actions ->
                    decideHardDouble (numericValue witness) dealerUpcard
            (SoftWitness, _) ->
                decideSoft (numericValue witness) dealerUpcard
            (HardWitness, _) ->
                decideHard (numericValue witness) dealerUpcard
            _ -> MHit

decideSoftStrategy :: Int -> Rank -> Bool -> Move
decideSoftStrategy playerTotal dealerRank canDouble = case (playerTotal, dealerRank) of
    (13, upcard) | canDouble && upcard `elem` [Five, Six] -> MDouble
    (14, upcard) | canDouble && upcard `elem` [Five, Six] -> MDouble
    (15, upcard) | canDouble && upcard `elem` [Four, Five, Six] -> MDouble
    (16, upcard) | canDouble && upcard `elem` [Four, Five, Six] -> MDouble
    (17, upcard) | canDouble && upcard `elem` [Three, Four, Five, Six] -> MDouble
    (18, upcard) | canDouble && upcard `elem` [Three, Four, Five, Six] -> MDouble
    _ -> decideSoftByRank playerTotal dealerRank

decideSoftByRank :: Int -> Rank -> Move
decideSoftByRank playerTotal dealerRank = case playerTotal of
    total | total >= 19 -> MStand
    18 -> case dealerRank of
        upcard | upcard `elem` [Nine, Ten, Ace] -> MHit
        _ -> MStand
    _ -> MHit

decideHardStrategy :: Int -> Rank -> Bool -> Bool -> Move
decideHardStrategy playerTotal dealerRank canDouble canSurrender
    | canSurrender = case (playerTotal, dealerRank) of
        (16, Ten) -> MSurrender
        (16, Nine) -> MSurrender
        (16, Ace) -> MSurrender
        (15, Ten) -> MSurrender
        _ -> decideHardByRank playerTotal dealerRank canDouble
    | otherwise = decideHardByRank playerTotal dealerRank canDouble

decideHardByRank :: Int -> Rank -> Bool -> Move
decideHardByRank playerTotal dealerRank canDouble = case (playerTotal, dealerRank) of
    (9, upcard) | canDouble && upcard `elem` [Three, Four, Five, Six] -> MDouble
    (10, upcard) | canDouble && upcard `notElem` [Ten, Ace] -> MDouble
    (11, upcard) | canDouble && upcard /= Ace -> MDouble
    _ -> case playerTotal of
        total | total >= 17 -> MStand
        total | total <= 11 -> MHit
        12 -> case dealerRank of
            upcard | upcard `elem` [Four, Five, Six] -> MStand
            _ -> MHit
        total | total `elem` [13, 14, 15, 16] -> case dealerRank of
            upcard | upcard `elem` [Two, Three, Four, Five, Six] -> MStand
            _ -> MHit
        _ -> MHit

decideSplit :: Rank -> Card -> Move
decideSplit pairRank dealerUpcard = case pairRank of
    Ace -> MSplit
    Eight -> MSplit
    Ten -> MStand
    Five -> MDouble
    rank' -> case (rank', rank dealerUpcard) of
        (Two, upcard) | upcard `elem` [Two, Three, Four, Five, Six, Seven] -> MSplit
        (Three, upcard) | upcard `elem` [Two, Three, Four, Five, Six, Seven] -> MSplit
        (Six, upcard) | upcard `elem` [Two, Three, Four, Five, Six] -> MSplit
        (Seven, upcard) | upcard `elem` [Two, Three, Four, Five, Six, Seven] -> MSplit
        (Nine, upcard) | upcard `notElem` [Seven, Ten, Ace] -> MSplit
        _ -> decideHard (rankValue pairRank * 2) dealerUpcard

decideSurrender :: Int -> Card -> Move
decideSurrender playerTotal dealerUpcard = case (playerTotal, rank dealerUpcard) of
    (16, Ten) -> MSurrender
    (16, Nine) -> MSurrender
    (16, Ace) -> MSurrender
    (15, Ten) -> MSurrender
    _ -> decideHard playerTotal dealerUpcard

decideSoftDouble :: Int -> Card -> Move
decideSoftDouble playerTotal dealerUpcard = case (playerTotal, rank dealerUpcard) of
    (13, upcard) | upcard `elem` [Five, Six] -> MDouble
    (14, upcard) | upcard `elem` [Five, Six] -> MDouble
    (15, upcard) | upcard `elem` [Four, Five, Six] -> MDouble
    (16, upcard) | upcard `elem` [Four, Five, Six] -> MDouble
    (17, upcard) | upcard `elem` [Three, Four, Five, Six] -> MDouble
    (18, upcard) | upcard `elem` [Three, Four, Five, Six] -> MDouble
    _ -> decideSoft playerTotal dealerUpcard

decideHardDouble :: Int -> Card -> Move
decideHardDouble playerTotal dealerUpcard = case (playerTotal, rank dealerUpcard) of
    (9, upcard) | upcard `elem` [Three, Four, Five, Six] -> MDouble
    (10, upcard) | upcard `notElem` [Ten, Ace] -> MDouble
    (11, upcard) | upcard /= Ace -> MDouble
    _ -> decideHard playerTotal dealerUpcard

decideSoft :: Int -> Card -> Move
decideSoft playerTotal dealerUpcard = case playerTotal of
    total | total >= 19 -> MStand
    18 -> case rank dealerUpcard of
        upcard | upcard `elem` [Nine, Ten, Ace] -> MHit
        _ -> MStand
    _ -> MHit

decideHard :: Int -> Card -> Move
decideHard playerTotal dealerUpcard = case playerTotal of
    total | total >= 17 -> MStand
    total | total <= 11 -> MHit
    12 -> case rank dealerUpcard of
        upcard | upcard `elem` [Four, Five, Six] -> MStand
        _ -> MHit
    total | total `elem` [13, 14, 15, 16] -> case rank dealerUpcard of
        upcard | upcard `elem` [Two, Three, Four, Five, Six] -> MStand
        _ -> MHit
    _ -> MHit

validateMove :: SomeHand -> Move -> GameRuleSet -> Int -> Bool
validateMove hand move rules splitCount =
    let contextWitness = computeContextualWitness (handCards hand) rules splitCount
     in isLegalMove move (SomeHand (handCards hand) contextWitness) rules

strategyChartLookup :: SomeHand -> Card -> GameRuleSet -> Int -> (Move, String)
strategyChartLookup hand dealerUpcard rules splitCount =
    let contextWitness = computeContextualWitness (handCards hand) rules splitCount
        move = basicStrategy hand dealerUpcard rules splitCount
        reasoning = explainStrategy contextWitness dealerUpcard
     in (move, reasoning)

explainStrategy :: HandWitness -> Card -> String
explainStrategy witness dealerUpcard = case (valueType witness, structure witness) of
    (BlackjackWitness, _) -> "Blackjack - always stand"
    (BustWitness, _) -> "Already busted"
    (_, PairWitness rank)
        | CanSplitWitness `elem` availableActions witness ->
            "Pair of " ++ show rank ++ "s vs " ++ show dealerUpcard
    (SoftWitness, _) ->
        "Soft " ++ show (numericValue witness) ++ " vs " ++ show dealerUpcard
    (HardWitness, _) ->
        "Hard " ++ show (numericValue witness) ++ " vs " ++ show dealerUpcard

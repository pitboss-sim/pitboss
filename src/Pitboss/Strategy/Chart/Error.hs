module Pitboss.Strategy.Chart.Error where

import Pitboss.Blackjack.Materia.Card (Rank)
import Pitboss.Blackjack.Materia.Hand (HandKind)
import Text.Printf (printf)
import Pitboss.Strategy.Chart.Types (kindToHandPrefix)

data ChartParseErrorReason
    = InvalidRowPrefix String
    | UnreadableHardTotal String
    | OutOfRangeHardTotal Int
    | UnknownStrategyCode String
    | WrongTokenCount Int Int
    | UnexpectedTokenInRow String
    | CouldNotParseHandPrefix String
    | CouldNotParseUpcard String
    deriving (Eq, Show)

data Pos = Pos
    { line :: Int
    , column :: Maybe Int
    }
    deriving (Eq, Show)

data ChartParseError = ChartParseError
    { errPos :: Pos
    , errContent :: String
    , errReason :: ChartParseErrorReason
    }
    deriving (Eq, Show)

prettyChartParseError :: ChartParseError -> String
prettyChartParseError (ChartParseError (Pos l c) content reason) =
    let loc = case c of
            Just col -> printf "❌ Parse error at line %d, column %d:" l col
            Nothing -> printf "❌ Parse error at line %d:" l
        reasonMsg = case reason of
            InvalidRowPrefix str ->
                "Invalid row prefix: \"" ++ str ++ "\". Expected one of: H, A, P, PA, PT."
            UnreadableHardTotal str ->
                "Cannot read hard total from: \"" ++ str ++ "\"."
            OutOfRangeHardTotal n ->
                "Hard total out of valid range: " ++ show n
            UnknownStrategyCode code ->
                "Unknown strategy code: \"" ++ code ++ "\"."
            WrongTokenCount expected actual ->
                "Wrong number of tokens: expected " ++ show expected ++ ", got " ++ show actual
            UnexpectedTokenInRow str ->
                "Unexpected token in row: \"" ++ str ++ "\""
            CouldNotParseHandPrefix str ->
                "Could not parse hand prefix from: \"" ++ str ++ "\"."
            CouldNotParseUpcard str ->
                "Could not parse upcard from: \"" ++ str ++ "\"."
        pointerLine = case c of
            Just col -> "  " ++ replicate (max 0 (col - 1)) ' ' ++ "^"
            Nothing -> ""
     in unlines
            [ loc
            , "  " ++ content
            , pointerLine
            , "→ " ++ reasonMsg
            ]

prettyChartParseErrors :: [ChartParseError] -> String
prettyChartParseErrors = unlines . map prettyChartParseError

prettyValidationError :: (HandKind, Maybe Int, Rank) -> String
prettyValidationError (kind, value, up) =
    let prefix = kindToHandPrefix kind value
    in "  Undefined move for hand: " ++ show prefix ++ ", dealer upcard: " ++ show up

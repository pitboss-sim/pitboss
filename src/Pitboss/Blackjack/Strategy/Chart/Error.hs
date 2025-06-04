{-# LANGUAGE LambdaCase #-}

module Pitboss.Blackjack.Strategy.Chart.Error where

import Pitboss.Blackjack.Strategy.Chart.Types
import Pitboss.Blackjack.Types.Core
import Text.Printf (printf)

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
            Just col -> printf "❌ Line %d, column %d:" l col
            Nothing -> printf "❌ Line %d:" l
        reasonMsg = prettyErrorReason reason
        pointerLine = case c of
            Just col -> "  " ++ replicate (max 0 (col - 1)) ' ' ++ "^"
            Nothing -> ""
        suggestion = errorSuggestion reason
     in unlines $
            filter
                (not . null)
                [ loc
                , "  " ++ content
                , pointerLine
                , "→ " ++ reasonMsg
                , suggestion
                ]

prettyErrorReason :: ChartParseErrorReason -> String
prettyErrorReason = \case
    InvalidRowPrefix str ->
        "Invalid row prefix: \"" ++ str ++ "\""
    UnreadableHardTotal str ->
        "Cannot read hard total from: \"" ++ str ++ "\""
    OutOfRangeHardTotal n ->
        printf "Hard total %d is out of valid range (5-21)" n
    UnknownStrategyCode code ->
        "Unknown strategy code: \"" ++ code ++ "\""
    WrongTokenCount expected actual ->
        printf "Expected %d strategy codes, but found %d" expected actual
    UnexpectedTokenInRow str ->
        "Unexpected token: \"" ++ str ++ "\""
    CouldNotParseHandPrefix str ->
        "Could not parse hand prefix: \"" ++ str ++ "\""
    CouldNotParseUpcard str ->
        "Could not parse dealer upcard: \"" ++ str ++ "\""

errorSuggestion :: ChartParseErrorReason -> String
errorSuggestion = \case
    InvalidRowPrefix _ ->
        "💡 Valid prefixes: H5-H21 (hard totals), A13-A21 (soft totals), P2-P10,PA (pairs), PT (blackjack)"
    UnknownStrategyCode _ ->
        "💡 Valid codes: H (hit), S (stand), D (double/hit), d (double/stand), V (split), v (split/hit), r (surrender/stand), . (undefined)"
    WrongTokenCount expected _ ->
        printf "💡 Each row needs exactly %d codes for dealer upcards: 2, 3, 4, 5, 6, 7, 8, 9, 10, A" expected
    OutOfRangeHardTotal n
        | n < 5 ->
            "💡 Hard totals below 5 are impossible with valid blackjack hands"
    OutOfRangeHardTotal n
        | n > 21 ->
            "💡 Hard totals above 21 are busted hands (use H22+ if needed for special cases)"
    UnreadableHardTotal _ ->
        "💡 Hard totals should be numbers like H17, H20, etc."
    CouldNotParseHandPrefix _ ->
        "💡 Examples: H17 (hard 17), A18 (soft 18), P8 (pair of 8s), PA (pair of aces), PT (blackjack)"
    _ -> ""

prettyChartParseErrors :: [ChartParseError] -> String
prettyChartParseErrors [] = "✅ No parsing errors found"
prettyChartParseErrors [err] = prettyChartParseError err
prettyChartParseErrors errs =
    let count = length errs
        header = printf "Found %d parsing errors:\n" count
        body = unlines (map prettyChartParseError errs)
        footer = "\n💡 Fix these errors and try again"
     in header ++ body ++ footer

prettyValidationError :: (HandKind, Maybe Int, Rank) -> String
prettyValidationError (kind, value', up) =
    let prefix = kindToHandPrefix kind value'
        suggestion = "Consider adding a strategy code or use '.' for explicitly undefined"
     in printf
            "  ❌ Missing strategy for %s vs dealer %s\n     💡 %s"
            (show prefix)
            (show up)
            suggestion

prettyValidationErrors :: [(HandKind, Maybe Int, Rank)] -> String
prettyValidationErrors [] = "✅ Strategy chart is complete"
prettyValidationErrors errs =
    let count = length errs
        header = printf "Found %d undefined strategies:\n" count
        body = unlines (map prettyValidationError errs)
        footer = "\n💡 Add strategy codes for these situations or mark as undefined with '.'"
     in header ++ body ++ footer

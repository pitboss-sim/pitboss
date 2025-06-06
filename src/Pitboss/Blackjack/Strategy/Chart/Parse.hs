module Pitboss.Blackjack.Strategy.Chart.Parse where

import Data.Char (isDigit)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Pitboss.Blackjack.Strategy.Chart.Error
import Pitboss.Blackjack.Strategy.Chart.Types
import Pitboss.Blackjack.Types.Core

parseStrategyChart :: Text -> Either [ChartParseError] StrategyChart
parseStrategyChart input =
    let nonEmptyLines = [(lnum, raw) | (lnum, raw) <- zip [1 ..] (T.lines input), not (isComment raw)]
     in case nonEmptyLines of
            [] -> Left [ChartParseError (Pos 1 Nothing) "" (InvalidRowPrefix "empty file")]
            _ -> sequence [either (Left . pure) Right (parseLine lnum raw) | (lnum, raw) <- nonEmptyLines]

isComment :: Text -> Bool
isComment t =
    let stripped = T.strip t
     in T.null stripped || T.pack "#" `T.isPrefixOf` stripped

parseLine :: Int -> Text -> Either ChartParseError ChartEntry
parseLine lnum fullLine =
    let raw = T.takeWhile (/= '#') fullLine
        tokens = T.words raw
     in case tokens of
            [] -> Left $ ChartParseError (Pos lnum Nothing) (T.unpack fullLine) (InvalidRowPrefix "empty line")
            (prefixTxt : moveToks) -> do
                prefix <- parsePrefix lnum prefixTxt fullLine
                let (kind, value') = handPrefixToKind prefix
                validateTokenCount lnum fullLine moveToks >>= \validToks -> do
                    let tokenColumns = actualTokenColumns raw validToks
                    moves' <- sequence [parseToken lnum fullLine col tok | (col, tok) <- tokenColumns]
                    let moveMap = Map.fromList (zip dealerRanks moves')
                    pure $ ChartEntry kind value' moveMap

validateTokenCount :: Int -> Text -> [Text] -> Either ChartParseError [Text]
validateTokenCount lnum fullLine moveToks =
    case compare (length moveToks) 10 of
        EQ -> Right moveToks
        LT ->
            Left $
                ChartParseError
                    (Pos lnum Nothing)
                    (T.unpack fullLine)
                    (WrongTokenCount 10 (length moveToks))
        GT ->
            Left $
                ChartParseError
                    (Pos lnum Nothing)
                    (T.unpack fullLine)
                    (WrongTokenCount 10 (length moveToks))

parseDigitSuffix :: Int -> Text -> String -> Either ChartParseError Int
parseDigitSuffix lnum fullLine str
    | all isDigit str && not (null str) = Right (read str)
    | otherwise =
        Left $
            ChartParseError
                (Pos lnum Nothing)
                (T.unpack fullLine)
                (UnreadableHardTotal str)

validateRange :: Int -> Text -> Int -> Int -> Int -> Either ChartParseError Int
validateRange lnum fullLine value' minVal maxVal
    | value' >= minVal && value' <= maxVal = Right value'
    | otherwise =
        Left $
            ChartParseError
                (Pos lnum Nothing)
                (T.unpack fullLine)
                (OutOfRangeHardTotal value')

parsePrefix :: Int -> Text -> Text -> Either ChartParseError HandPrefix
parsePrefix lnum txt fullLine =
    case T.unpack txt of
        "PA" -> Right PA
        "PT" -> Right PT
        'P' : ns -> do
            n <- parseDigitSuffix lnum fullLine ns
            _ <- validateRange lnum fullLine n 2 10
            pure (P n)
        'A' : ns -> do
            nonAceValue <- parseDigitSuffix lnum fullLine ns
            _ <- validateRange lnum fullLine nonAceValue 2 9
            pure (A (11 + nonAceValue))
        'H' : ns -> do
            n <- parseDigitSuffix lnum fullLine ns
            _ <- validateRange lnum fullLine n 4 21
            pure (H n)
        ns | all isDigit ns && not (null ns) -> do
            n <- parseDigitSuffix lnum fullLine ns
            _ <- validateRange lnum fullLine n 4 21
            pure (H n)
        _ ->
            Left $
                ChartParseError
                    (Pos lnum Nothing)
                    (T.unpack fullLine)
                    (InvalidRowPrefix (T.unpack txt))

parseToken :: Int -> Text -> Int -> Text -> Either ChartParseError MoveCode
parseToken lnum fullLine col t =
    case T.unpack t of
        "H" -> Right MoveHit
        "S" -> Right MoveStand
        "D" -> Right MoveDoubleOrHit
        "d" -> Right MoveDoubleOrStand
        "V" -> Right MoveSplit
        "v" -> Right MoveSplitOrHit
        "r" -> Right MoveSurrenderOrStand
        "." -> Right MoveUndefined
        other ->
            Left $
                ChartParseError
                    (Pos lnum (Just col))
                    (T.unpack fullLine)
                    (UnknownStrategyCode other)

actualTokenColumns :: Text -> [Text] -> [(Int, Text)]
actualTokenColumns line' tokens = go (T.unpack line') tokens 1
  where
    go _ [] _ = []
    go [] _ _ = []
    go (c : cs) ts@(t : rest) idx
        | Just restLine <- stripPrefix (T.unpack t) (c : cs) =
            (idx, t) : go restLine rest (idx + length (T.unpack t))
        | c == ' ' || c == '\t' =
            go cs ts (idx + 1)
        | otherwise =
            go cs ts (idx + 1)

    stripPrefix :: String -> String -> Maybe String
    stripPrefix [] ys = Just ys
    stripPrefix (x : xs) (y : ys)
        | x == y = stripPrefix xs ys
    stripPrefix _ _ = Nothing

dealerRanks :: [Rank]
dealerRanks = [Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Ace]

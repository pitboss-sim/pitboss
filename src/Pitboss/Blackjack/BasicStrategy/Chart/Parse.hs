module Pitboss.Blackjack.BasicStrategy.Chart.Parse where

import Data.Char (isDigit)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Pitboss.Blackjack.Materia.Card (Rank (..))
import Pitboss.Blackjack.BasicStrategy.Chart.Error
import Pitboss.Blackjack.BasicStrategy.Chart.Types

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
                let (kind, value) = handPrefixToKind prefix
                validateTokenCount lnum fullLine moveToks >>= \validToks -> do
                    let tokenColumns = actualTokenColumns raw validToks
                    moves' <- sequence [parseToken lnum fullLine col tok | (col, tok) <- tokenColumns]
                    let moveMap = Map.fromList (zip dealerRanks moves')
                    pure $ ChartEntry kind value moveMap

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

parsePrefix :: Int -> Text -> Text -> Either ChartParseError HandPrefix
parsePrefix lnum txt fullLine =
    case T.unpack txt of
        "PA" -> Right PA
        "PT" -> Right PT
        'P' : ns
            | all isDigit ns
            , not (null ns) ->
                let n = read ns
                 in if n >= 2 && n <= 10
                        then Right (P n)
                        else
                            Left $
                                ChartParseError
                                    (Pos lnum Nothing)
                                    (T.unpack fullLine)
                                    (OutOfRangeHardTotal n)
        'A' : ns
            | all isDigit ns
            , not (null ns) ->
                let n = read ns
                 in if n >= 13 && n <= 21
                        then Right (A n)
                        else
                            Left $
                                ChartParseError
                                    (Pos lnum Nothing)
                                    (T.unpack fullLine)
                                    (OutOfRangeHardTotal n)
        'H' : ns
            | all isDigit ns
            , not (null ns) ->
                let n = read ns
                 in if n >= 5 && n <= 21
                        then Right (H n)
                        else
                            Left $
                                ChartParseError
                                    (Pos lnum Nothing)
                                    (T.unpack fullLine)
                                    (OutOfRangeHardTotal n)
        ns
            | all isDigit ns
            , not (null ns) ->
                let n = read ns
                 in if n >= 5 && n <= 21
                        then Right (H n)
                        else
                            Left $
                                ChartParseError
                                    (Pos lnum Nothing)
                                    (T.unpack fullLine)
                                    (OutOfRangeHardTotal n)
        'H' : ns ->
            Left $
                ChartParseError
                    (Pos lnum Nothing)
                    (T.unpack fullLine)
                    (UnreadableHardTotal ns)
        'A' : ns ->
            Left $
                ChartParseError
                    (Pos lnum Nothing)
                    (T.unpack fullLine)
                    (UnreadableHardTotal ns)
        'P' : ns ->
            Left $
                ChartParseError
                    (Pos lnum Nothing)
                    (T.unpack fullLine)
                    (UnreadableHardTotal ns)
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

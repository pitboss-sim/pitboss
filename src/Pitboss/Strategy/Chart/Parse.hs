module Pitboss.Strategy.Chart.Parse where

import Data.Char (isDigit)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Pitboss.Blackjack.Card (Rank (..))
import Pitboss.Strategy.Chart.Error
import Pitboss.Strategy.Chart.Types

parseStrategyChart :: Text -> Either [ChartParseError] StrategyChart
parseStrategyChart input =
    sequence
        [ either (Left . pure) Right (parseLine lnum raw)
        | (lnum, raw) <- zip [1 ..] (T.lines input)
        , not (isComment raw)
        ]

isComment :: Text -> Bool
isComment t = T.pack "#" `T.isPrefixOf` T.strip t || T.strip t == T.empty

parseLine :: Int -> Text -> Either ChartParseError ChartEntry
parseLine lnum fullLine =
    let raw = T.takeWhile (/= '#') fullLine
        tokens = T.words raw
     in case tokens of
            [] ->
                Left $
                    ChartParseError
                        (Pos lnum Nothing)
                        (T.unpack fullLine)
                        (InvalidRowPrefix "")
            (prefixTxt : moveToks) -> do
                prefix <- parsePrefix lnum prefixTxt fullLine
                let (kind, value) = handPrefixToKind prefix
                case compare (length moveToks) 10 of
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
                    EQ -> do
                        let tokenColumns = actualTokenColumns raw moveToks
                        moves' <-
                            sequence
                                [ parseToken lnum fullLine col tok
                                | (col, tok) <- tokenColumns
                                ]
                        let moveMap = Map.fromList (zip dealerRanks moves')
                        pure $ ChartEntry kind value moveMap

parsePrefix :: Int -> Text -> Text -> Either ChartParseError HandPrefix
parsePrefix lnum txt raw = case T.unpack txt of
    "PA" -> Right PA
    "PT" -> Right PT
    'P' : ns | all isDigit ns -> Right $ P (read ns)
    'A' : ns | all isDigit ns -> Right $ A (read ns)
    ns | all isDigit ns -> Right $ H (read ns)
    _ ->
        Left $
            ChartParseError
                (Pos lnum Nothing)
                (T.unpack raw)
                (InvalidRowPrefix (T.unpack txt))

parseToken :: Int -> Text -> Int -> Text -> Either ChartParseError MoveCode
parseToken lnum raw col t =
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
                    (T.unpack raw)
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

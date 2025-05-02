module Pitboss.Strategy.Chart.Validate where

import Data.Map.Strict (toList)
import Pitboss.Blackjack.Materia.Card (Rank)
import Pitboss.Blackjack.Materia.Hand (HandKind)
import Pitboss.Strategy.Chart.Error (prettyValidationErrors)
import Pitboss.Strategy.Chart.Types (ChartEntry (..), MoveCode (..))

validateStrategyChart :: [ChartEntry] -> [(HandKind, Maybe Int, Rank)]
validateStrategyChart =
    concatMap validateEntry
  where
    validateEntry (ChartEntry kind value moves') =
        [ (kind, value, up)
        | (up, mv) <- toList moves'
        , mv == MoveUndefined
        ]

validateAndReport :: [ChartEntry] -> String
validateAndReport chart =
    let errors = validateStrategyChart chart
     in prettyValidationErrors errors

checkCompleteness :: [ChartEntry] -> IO ()
checkCompleteness chart = do
    let report = validateAndReport chart
    putStrLn report
    case validateStrategyChart chart of
        [] -> putStrLn "✅ Strategy chart validation passed"
        errs -> do
            putStrLn $ "❌ Found " ++ show (length errs) ++ " undefined strategies"
            putStrLn "Use '.' to explicitly mark strategies as undefined if intentional"

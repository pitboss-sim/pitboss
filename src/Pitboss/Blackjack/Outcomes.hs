{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Pitboss.Blackjack.Outcomes where

import Data.Aeson.Types
import GHC.Generics (Generic)

data BoutOutcome
    = BoutPlayerWin
    | BoutDealerWin
    | Push
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data WinReason
    = HigherScore
    | OpponentBust
    | NaturalBlackjack
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data DetailedOutcome = DetailedOutcome
    { outcome :: BoutOutcome
    , reason :: Maybe WinReason
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

boutPlayerWinsHigher :: DetailedOutcome
boutPlayerWinsHigher = DetailedOutcome BoutPlayerWin (Just HigherScore)

boutPlayerWinsDealerBust :: DetailedOutcome
boutPlayerWinsDealerBust = DetailedOutcome BoutPlayerWin (Just OpponentBust)

boutPlayerWinsBlackjack :: DetailedOutcome
boutPlayerWinsBlackjack = DetailedOutcome BoutPlayerWin (Just NaturalBlackjack)

dealerWinsHigher :: DetailedOutcome
dealerWinsHigher = DetailedOutcome BoutDealerWin (Just HigherScore)

dealerWinsBoutPlayerBust :: DetailedOutcome
dealerWinsBoutPlayerBust = DetailedOutcome BoutDealerWin (Just OpponentBust)

dealerWinsBlackjack :: DetailedOutcome
dealerWinsBlackjack = DetailedOutcome BoutDealerWin (Just NaturalBlackjack)

pushOutcome :: DetailedOutcome
pushOutcome = DetailedOutcome Push Nothing

basicOutcome :: DetailedOutcome -> BoutOutcome
basicOutcome = outcome

{-# LANGUAGE DeriveGeneric #-}

module Pitboss.Blackjack.Outcomes where

import Data.Aeson.Types
import GHC.Generics (Generic)

data BoutOutcome
    = ContestantWin
    | DealerWin
    | Push
    deriving (Eq, Show, Generic)

instance ToJSON BoutOutcome
instance FromJSON BoutOutcome

data WinReason
    = HigherScore
    | OpponentBust
    | NaturalBlackjack
    deriving (Eq, Show, Generic)

instance ToJSON WinReason
instance FromJSON WinReason

data DetailedOutcome = DetailedOutcome
    { outcome :: BoutOutcome
    , reason :: Maybe WinReason
    }
    deriving (Eq, Show, Generic)

instance ToJSON DetailedOutcome
instance FromJSON DetailedOutcome

contestantWinsHigher :: DetailedOutcome
contestantWinsHigher = DetailedOutcome ContestantWin (Just HigherScore)

contestantWinsDealerBust :: DetailedOutcome
contestantWinsDealerBust = DetailedOutcome ContestantWin (Just OpponentBust)

contestantWinsBlackjack :: DetailedOutcome
contestantWinsBlackjack = DetailedOutcome ContestantWin (Just NaturalBlackjack)

dealerWinsHigher :: DetailedOutcome
dealerWinsHigher = DetailedOutcome DealerWin (Just HigherScore)

dealerWinsContestantBust :: DetailedOutcome
dealerWinsContestantBust = DetailedOutcome DealerWin (Just OpponentBust)

dealerWinsBlackjack :: DetailedOutcome
dealerWinsBlackjack = DetailedOutcome DealerWin (Just NaturalBlackjack)

pushOutcome :: DetailedOutcome
pushOutcome = DetailedOutcome Push Nothing

basicOutcome :: DetailedOutcome -> BoutOutcome
basicOutcome = outcome

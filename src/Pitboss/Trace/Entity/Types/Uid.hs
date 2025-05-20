{-# LANGUAGE DerivingStrategies #-}

module Pitboss.Trace.Entity.Types.Uid where

import Data.Char (toUpper)
import Numeric (showIntAtBase)
import Pitboss.Trace.Entity.Types
import System.Random (StdGen, randomR)

mkUid :: String -> Maybe Uid
mkUid s = if isValidUidString s then Just (Uid s) else Nothing

generateUid :: Int -> StdGen -> (Uid, StdGen)
generateUid counter gen =
    let (entropy, gen') = randomR (0, 2 ^ (40 :: Int) - 1) gen
        tsPart = showPaddedBase32 counter 6
        rndPart = showPaddedBase32 entropy 8
        str = tsPart ++ "-" ++ rndPart
     in case mkUid str of
            Just uid -> (uid, gen')
            Nothing -> error "Internal error: generated invalid Uid"

showPaddedBase32 :: Int -> Int -> String
showPaddedBase32 n width = padLeft '0' width (showIntAtBase 32 showBase32Digit n "")

showBase32Digit :: Int -> Char
showBase32Digit n
    | n >= 0 && n < length base32Chars = base32Chars !! n
    | otherwise = error $ "Invalid base32 digit: " ++ show n
  where
    base32Chars = "0123456789ABCDEFGHJKMNPQRSTVWXYZ"

padLeft :: Char -> Int -> String -> String
padLeft c width str = replicate (width - length str) c ++ str

-- validation

isValidUidString :: String -> Bool
isValidUidString str =
    case break (== '-') str of
        (prefix, '-' : suffix) ->
            length prefix == 6
                && length suffix == 8
                && all isBase32Char prefix
                && all isBase32Char suffix
        _ -> False

isBase32Char :: Char -> Bool
isBase32Char c = toUpper c `elem` "0123456789ABCDEFGHJKMNPQRSTVWXYZ"

parseUid :: String -> Maybe Uid
parseUid = mkUid

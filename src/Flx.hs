module Flx where

import Data.Char (toUpper)
import Data.Char
import Data.Maybe (fromMaybe)
import Data.List

wordSeparators = [' ', '-', '_', ':', '.', '/', '\\']

containsWordSeparators :: Char -> Bool
containsWordSeparators ch = elem ch wordSeparators

-- Check if `ch` is a word character.
isWord :: Maybe Char -> Bool
isWord ch =
    case ch of
        Nothing -> False
        Just ch -> (not (containsWordSeparators ch))

-- Check if` ch` is an uppercase character.
isCapital :: Maybe Char -> Bool
isCapital ch =
    case ch of
        Nothing -> False
        Just ch -> (isWord (Just ch)) && ch == (toUpper ch)

-- Check if `lastCh` is the end of a word and `ch` the start of the next.
isBoundary :: Maybe Char -> Char -> Bool
isBoundary lastCh ch
    | lastCh == Nothing = True
    | not (isCapital lastCh) && (isCapital (Just ch)) = True
    | not (isWord lastCh) && (isWord (Just ch)) = True
    | otherwise = False

-- Return best score matching `query` against `str`.
score :: String -> String -> [Int]
score str1 str2 =
    if str1 == [] then []
    else if str2 == [] then []
    else []

-- Program Main
main :: IO ()
main = do
    print(isCapital (Just 'C'))
    print(isCapital (Just 'c'))

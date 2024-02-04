-- Parsing.hs

module Parsing (anyDigit
               ,anyNotDigit
               ,anyAlpha
               ,anyNotAlpha
               ,anyWhite
               ,anyNotWhite
               ,optionalChar
               ,oneRangeRepetition
               ,orGroupRepetitionRange
               ,oneRepetition
               ,orGroupRepetition
               ,notOrRangeGroup
               ,notOrGroup
               ,range
               ,escapeChar
               ,pipeGroup
               ,anyChar
               ,normalChar
               ,orGroup) where

import Data.List (elemIndex)
import Data.Char (isDigit)
import Text.Read (readMaybe)
import CharFunctions(allAlphaNum)

-- Helpers --
splitOnNearest :: Char -> String -> (String, String)
splitOnNearest c str =
  case elemIndex c str of
    Just index ->  let (before, after) = splitAt index str in (before, tail after)
    Nothing -> ([],[])

readDigit :: String -> String -> (Int, String)
readDigit res [] = (read res, [])
readDigit res s
  | isDigit (head s) = readDigit (res ++ [head s]) (tail s)
  | otherwise = (read res, s)

-- Parisng --

anyDigit :: Char -> String -> Bool
anyNotDigit :: Char -> String -> Bool
anyAlpha :: Char -> String -> Bool
anyNotAlpha :: Char -> String -> Bool
anyWhite :: Char -> String -> Bool
anyNotWhite :: Char -> String -> Bool
optionalChar :: Char -> String -> Bool
oneRangeRepetition :: Char -> String -> Bool
orGroupRepetitionRange :: Char -> String -> Bool
oneRepetition :: Char -> String -> Bool
orGroupRepetition :: Char -> String -> Bool
notOrGroup :: Char -> String -> Bool
notOrRangeGroup :: Char -> String -> Bool
range :: Char -> String -> Bool

pipeGroup :: Char -> Bool
anyChar :: Char -> Bool
normalChar :: Char -> Bool
escapeChar :: Char -> Bool
orGroup :: Char -> Bool


-- Repetitions --
oneRangeRepetition f s = head s == '{' && head (snd (readDigit [] (tail s))) == '-'
orGroupRepetitionRange f s
  | not (orGroup f) = False
  | otherwise = do
    let (before, after) = splitOnNearest ']' s
    (after /= []) && (do
      oneRangeRepetition ' ' after)

oneRepetition f s = head s == '{'
orGroupRepetition f s
  | not (orGroup f) = False
  | otherwise = do
    let (before, after) = splitOnNearest ']' s
    (after /= []) && (do
      oneRepetition ' ' after)

-- Rest --
anyDigit f s = f =='\\' && head s == 'd'
anyNotDigit f s = f =='\\' && head s == 'D'
anyAlpha f s = f =='\\' && head s == 'w'
anyNotAlpha f s = f =='\\' && head s == 'W'
anyWhite f s = f =='\\' && head s == 's'
anyNotWhite f s = f =='\\' && head s == 'S'
optionalChar f s = f `elem` allAlphaNum && head s == '?'
notOrRangeGroup f s = notOrGroup f s && head (tail (tail s)) == '-'
notOrGroup f s = f =='[' && head s == '^'
range f s = f == '[' && head (tail s) == '-'

anyChar f = f == '.'
normalChar f = f `elem` allAlphaNum && f /= '\\'
escapeChar f = f == head "\\"
pipeGroup f = f =='('
orGroup f = f == '['
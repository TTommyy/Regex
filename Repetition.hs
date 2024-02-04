module Repetition
  (
    updateOneRangeRepetiton,
    updateOrGroupRangeRepetition,
    updateOneRepetition,
    updateOrGroupRepetition
  ) where

import Operations(addToSet, addAnyToSet,
                  addORGroupToSet, addNotORGroupToSet,
                  addORRangeGroupToSet, addNotORRangeGroupToSet,
                  addRepetitionsToSet, addRangeRepetitionsToSet,
                  addOptionalToSet, addMultipleRangeRepetitionsToSet,
                  generateRange)

import Data.Char (ord, chr, isAlphaNum, isAlpha, isDigit, isSpace)
import Data.List (elemIndex)

-- helper --
readDigit :: String -> String -> (Int, String)
readDigit res [] = (read res, [])
readDigit res s
  | isDigit (head s) = readDigit (res ++ [head s]) (tail s)
  | otherwise = (read res, s)

range :: Char -> String -> Bool
range f s = f == '[' && head (tail s) == '-'

splitOnNearest :: Char -> String -> (String, String)
splitOnNearest c str =
  case elemIndex c str of
    Just index ->  let (before, after) = splitAt index str in (before, tail after)
    Nothing -> ([],[])

updateOneRangeRepetiton :: [String] -> String -> [String]
updateOrGroupRangeRepetition :: [String] -> String -> [String]
updateOneRepetition :: [String] -> String -> [String]
updateOrGroupRepetition :: [String] -> String -> [String]

updateOneRangeRepetiton res (h:t) = do
  let (n, tt) = readDigit [] (tail t)
  let (m, _) = readDigit [] (tail tt)
  addRangeRepetitionsToSet res [h] (n, m)

updateOrGroupRangeRepetition res (h:t) = do
  let (before, after) = splitOnNearest ']' t
  let (n, tt) = readDigit [] (tail after)
  let (m, _) = readDigit [] (tail tt)
  if range h t then do
   addMultipleRangeRepetitionsToSet res (generateRange before) (n, m)
  else do
   addMultipleRangeRepetitionsToSet res before (n, m)

updateOneRepetition res (h:t) = do
  let (n, _) = readDigit [] (tail t)
  addRepetitionsToSet res [h] n

updateOrGroupRepetition res (h:t) = do
  let (before, after) = splitOnNearest ']' t
  let (n, tt) = readDigit [](tail after)
  if range h t then do
   addMultipleRangeRepetitionsToSet res (generateRange before) (n, n)
  else do
   addMultipleRangeRepetitionsToSet res before (n, n)

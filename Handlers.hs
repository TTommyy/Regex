module Handlers(OperationType(ADD, ANY_DIGIT, ANY_NOT_DIGIT, ANY_ALPHA,
                     ANY_NOT_ALPHA, ANY_WHITE, ANY_NOT_WHITE,
                     ADD_ANY, ADD_OPT, ADD_OR, ADD_NOT_OR,
                     ADD_NOT_OR_RANGE,
                     ADD_RANGE, ADD_ONE_RANGE_REPETITION,
                     ADD_OR_GROUP_RANGE_REPETITION,
                     ADD_ONE_REPETITION, ADD_OR_GROUP_REPETITION,
                     ADD_PIPE_GROUP ,ESCAPE),
                handleOneRangeRepetition,
                handleOrGroupRangeRepetition,handleOneRepetition,
                handleOrGroupRepetition, handleNotOrGroup, handleNotOrRangeGroup,
                handlePipeGroup, handleOrGroup,
                handleOrdinaryOrRangeGroup) where

import Data.Char (ord, chr, isAlphaNum, isAlpha, isDigit, isSpace)
import Data.List (elemIndex)

data OperationType = ADD| ANY_DIGIT| ANY_NOT_DIGIT| ANY_ALPHA|
                     ANY_NOT_ALPHA| ANY_WHITE| ANY_NOT_WHITE|
                     ADD_ANY| ADD_OPT| ADD_OR| ADD_NOT_OR| ADD_NOT_OR_RANGE|
                     ADD_RANGE| ADD_ONE_RANGE_REPETITION|
                     ADD_OR_GROUP_RANGE_REPETITION|
                     ADD_ONE_REPETITION| ADD_OR_GROUP_REPETITION|
                     ADD_PIPE_GROUP| ESCAPE
                deriving (Eq)

handleOneRangeRepetition :: String -> (OperationType, String, String)
handleOrGroupRangeRepetition :: String -> (OperationType, String, String)
handleOneRepetition :: String -> (OperationType, String, String)
handleOrGroupRepetition :: String -> (OperationType, String, String)

handleNotOrGroup :: String -> (OperationType, String, String)
handlePipeGroup :: String -> (OperationType, String, String)
handleOrGroup :: String -> (OperationType, String, String)
handleNotOrRangeGroup :: String -> (OperationType, String, String)
handleOrdinaryOrRangeGroup :: String -> (OperationType, String, String)

-- helpers --
splitOnNearest :: Char -> String -> (String, String)
splitOnNearest c str =
  case elemIndex c str of
    Just index ->  let (before, after) = splitAt index str in (before, tail after)
    Nothing -> error ("Closing of " ++ [c] ++ " not found. Wrong regex")

findClosingParenthees :: (Int, Int) -> String -> String -> (String, String)
findClosingParenthees (_, _) [] acc = error ("(" ++ acc ++ " have no closing )")
findClosingParenthees (op, cl) (h:t) acc
  | h == ')' && (op - 1) == cl = (acc, t)
  | h == ')' = findClosingParenthees (op, cl + 1) t (acc++[h])
  | h == '(' = findClosingParenthees (op + 1, cl) t (acc++[h])
  | otherwise = findClosingParenthees (op, cl) t (acc++[h])



-- Handlers --

handleOneRangeRepetition regex = do
  let (before, after) = splitOnNearest '}' regex
  (ADD_ONE_RANGE_REPETITION, before, after)

handleOrGroupRangeRepetition regex = do
  let (before, after) = splitOnNearest '}' regex
  (ADD_OR_GROUP_RANGE_REPETITION, before, after)

handleOneRepetition regex = do
  let (before, after) = splitOnNearest '}' regex
  (ADD_ONE_REPETITION, before, after)

handleOrGroupRepetition regex = do
  let (before, after) = splitOnNearest '}' regex
  (ADD_OR_GROUP_REPETITION, before, after)

handleNotOrGroup regex = do
  let (before, after) = splitOnNearest ']' regex
  (ADD_NOT_OR, before, after)

handleNotOrRangeGroup regex = do
  let (before, after) = splitOnNearest ']' regex
  (ADD_NOT_OR_RANGE, before, after)

handlePipeGroup regex = do
  let (before, after) = findClosingParenthees (1, 0) regex []
  (ADD_PIPE_GROUP, before, after)

handleOrGroup regex = do
  let (before, after) = splitOnNearest ']' regex
  (ADD_OR, before, after)

handleOrdinaryOrRangeGroup regex = do
  let (before, after) = splitOnNearest ']' regex
  (ADD_RANGE, before, after)
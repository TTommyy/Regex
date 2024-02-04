-- Regex.hs
module Regex(generateSetFromRegex) where

import Data.Char (ord
                 ,chr
                 ,isAlphaNum
                 ,isAlpha
                 ,isDigit
                 ,isSpace)

import Data.List (elemIndex)

import Parsing (anyDigit
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
               ,orGroup)

import Operations
      (addToSet
      ,addAnyToSet
      ,addORGroupToSet
      ,addNotORGroupToSet
      ,addORRangeGroupToSet
      ,addNotORRangeGroupToSet
      ,addRepetitionsToSet
      ,addRangeRepetitionsToSet
      ,addOptionalToSet
      ,generateRange)

import Handlers
    (OperationType(ADD
                  ,ANY_DIGIT
                  ,ANY_NOT_DIGIT
                  ,ANY_ALPHA
                  ,ANY_NOT_ALPHA
                  ,ANY_WHITE
                  ,ANY_NOT_WHITE
                  ,ADD_ANY
                  ,ADD_OPT
                  ,ADD_OR
                  ,ADD_NOT_OR
                  ,ADD_NOT_OR_RANGE
                  ,ADD_RANGE
                  ,ADD_ONE_RANGE_REPETITION
                  ,ADD_OR_GROUP_RANGE_REPETITION
                  ,ADD_ONE_REPETITION
                  ,ADD_OR_GROUP_REPETITION
                  ,ADD_PIPE_GROUP
                  ,ESCAPE)
      ,handleOneRangeRepetition
      ,handleOrGroupRangeRepetition
      ,handleOneRepetition
      ,handleOrGroupRepetition
      ,handleNotOrRangeGroup
      ,handleNotOrGroup
      ,handlePipeGroup
      ,handleOrGroup
      ,handleOrdinaryOrRangeGroup)

import Repetition(updateOneRangeRepetiton
                 ,updateOrGroupRangeRepetition
                 ,updateOneRepetition
                 ,updateOrGroupRepetition)

allASCIICharacters :: String
allAlphaNum :: String
letters :: String
digits :: String
whiteSpaces :: String

allASCIICharacters = map chr [0..127]
allAlphaNum = filter isAlphaNum allASCIICharacters
letters = filter isAlpha allASCIICharacters
digits =  filter isDigit allASCIICharacters
whiteSpaces = filter isSpace allASCIICharacters

-- Helpers --
splitOnNearest :: Char -> String -> (String, String)
splitOnNearest c str =
  case elemIndex c str of
    Just index ->  let (before, after) = splitAt index str in (before, tail after)
    Nothing -> error ("Closing of " ++ [c] ++ " not found. Wrong regex")

-- Genertion --
match :: String -> (OperationType, String,String) -- type, group, rest
match [a]
  | normalChar a = (ADD, [a], [])
  | anyChar a = (ADD_ANY, [a], [])
  | otherwise = error ("NOT SUPPORTED: " ++ [a])
match (h : t)
-- Firstly we check most sofisticated repetitions.
  | oneRangeRepetition h t = handleOneRangeRepetition (h:t)
  | orGroupRepetitionRange h t = handleOrGroupRangeRepetition (h:t)
  | oneRepetition h t = handleOneRepetition (h:t)
  | orGroupRepetition h t  = handleOrGroupRepetition (h:t)
-- Then 2 args fun --
  | anyDigit h t = (ANY_DIGIT, [head t], tail t)
  | anyNotDigit h t = (ANY_NOT_DIGIT, [head t], tail t)
  | anyAlpha h t = (ANY_ALPHA, [head t], tail t)
  | anyNotAlpha h t = (ANY_NOT_ALPHA, [head t], tail t)
  | anyWhite h t = (ANY_WHITE, [head t], tail t)
  | anyNotWhite h t = (ANY_NOT_WHITE, [head t], tail t)
  | optionalChar h t  = (ADD_OPT, [h], tail t)
  | range h t = handleOrdinaryOrRangeGroup t
  | notOrRangeGroup h t = handleNotOrRangeGroup (tail t)
  | notOrGroup h t = handleNotOrGroup (tail t)
  -- and one args fun
  | pipeGroup h = handlePipeGroup t
  | anyChar h = (ADD_ANY, [h], t)
  | normalChar h = (ADD, [h], t)
  | orGroup h = handleOrGroup t
  | escapeChar h = (ESCAPE, [head t], tail t)
  | otherwise = error ("NOT SUPPORTED: " ++ [h] ++ t)


findfirstGoodPipe :: (Int, Int) -> String -> String -> (String, String)
findfirstGoodPipe (_, _) [] acc = error (acc ++ " have no Pipe |")
findfirstGoodPipe (op, cl) (h:t) acc
  | h == '(' = findfirstGoodPipe (op + 1, cl) t (acc++[h])
  | h == ')' = findfirstGoodPipe (op, cl + 1) t (acc++[h])
  | h == '|' && op == cl = (acc, t)
  | otherwise = findfirstGoodPipe (op, cl) t (acc++[h])

splitOn :: Char -> String -> String -> [String] -> [String]
splitOn _ [] acc res = acc : res
splitOn c (h:t) acc res = do
  if h == '|' then splitOn c t [] [acc]++res
  else splitOn c t (acc++[h]) res

updatePipeGroup :: [String] -> String -> [String]
updatePipeGroup res pipeGroup
  | '|' `notElem` pipeGroup = error "Wrong regex! () dont have | inside!"
  | '(' `notElem` pipeGroup = do
      let groups = splitOn '|' pipeGroup [] []
      concatMap (generateSetFromRegex res) groups
  | otherwise = do
      let (f,s) = findfirstGoodPipe (0, 0) pipeGroup []
      generateSetFromRegex res f ++ generateSetFromRegex res s

updateSet :: [String] -> (OperationType, String) -> [String]
updateSet res (operation, group)
  | operation == ADD_ONE_RANGE_REPETITION = updateOneRangeRepetiton res group
  | operation == ADD_OR_GROUP_RANGE_REPETITION = updateOrGroupRangeRepetition res group
  | operation == ADD_ONE_REPETITION = updateOneRepetition res group
  | operation == ADD_OR_GROUP_REPETITION = updateOrGroupRepetition res group
  | operation == ADD = Operations.addToSet res group
  | operation == ANY_DIGIT = Operations.addORGroupToSet res digits
  | operation == ANY_NOT_DIGIT = Operations.addNotORGroupToSet res digits
  | operation == ANY_ALPHA = Operations.addORGroupToSet res allAlphaNum
  | operation == ANY_NOT_ALPHA = Operations.addNotORGroupToSet res allAlphaNum
  | operation == ANY_WHITE  = Operations.addORGroupToSet res whiteSpaces
  | operation == ANY_NOT_WHITE = Operations.addNotORGroupToSet res whiteSpaces
  | operation == ADD_ANY = Operations.addORGroupToSet res allASCIICharacters
  | operation == ADD_OPT = Operations.addOptionalToSet res group
  | operation == ADD_OR = Operations.addORGroupToSet res group
  | operation == ADD_NOT_OR = Operations.addNotORGroupToSet res group
  | operation == ADD_NOT_OR_RANGE = Operations.addNotORRangeGroupToSet res group
  | operation == ADD_RANGE = Operations.addORRangeGroupToSet res group
  | operation == ADD_PIPE_GROUP = updatePipeGroup res group
  | operation == ESCAPE = Operations.addToSet res group

generateSetFromRegex :: [String] -> String -> [String]
generateSetFromRegex res [] = res
generateSetFromRegex res regex = do
  let (t, gr, rest) = match regex
  generateSetFromRegex (updateSet res (t, gr)) rest
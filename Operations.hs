-- Operations.hs

--  abc.. Letters done addToSet
-- 123…	Digits done addToSet
-- \d	Any Digit done addORGroupToSet
-- \D	Any Non-digit characteraddORGroupToSet
-- .	Any Character addORGroupToSet
-- \.	Period included in letters
-- [abc]	Only a, b, or c done addORGroupToSet
-- [^abc]	Not a, b, nor c done addNotORGroupToSet
-- [a-z]	Characters a to z done addRangeToSet
-- [0-9]	Numbers 0 to 9 done addRangeToSet
-- \w	Any Alphanumeric character done addORGroupToSet
-- \W	Any Non-alphanumeric character addNotORGroupToSet
-- {m}	m Repetitions done AddRepetitionToSet
-- {m,n}	m to n Repetitions AddRangeRepetitionToSet
-- *	Zero or more repetitions --those are excluded due to reqiermet of finity
-- +	One or more repetitions -- those are excluded due to reqiermet of finity
-- ?	Optional character do addOptionalToSet
-- \s	Any Whitespace done addOrGroup
-- \S	Any Non-whitespace character done addNotOrGroup
-- ^…$	Starts and ends those are excluded due to reqiermet of finity
-- (…)	Capture Group does not make sens in terms of defining set
-- (a(bc))	Capture Sub-group does not make sens in terms of defining set
-- (.*)	Capture all does not make sens in terms of defining set
-- (abc|def)	Matches abc or def done in Regex.hs

module Operations(addToSet
                 ,addAnyToSet
                 ,addORGroupToSet
                 ,addNotORGroupToSet
                 ,addORRangeGroupToSet
                 ,addNotORRangeGroupToSet
                 ,addRepetitionsToSet
                 ,addRangeRepetitionsToSet
                 ,addOptionalToSet
                 ,addMultipleRangeRepetitionsToSet
                 ,generateRange) where

import Data.Char (ord
                 ,chr)

import CharFunctions(allASCIICharacters)

-- Hepler methods --
notGroup      :: String -> String
generateRange :: String -> String
duplicate     :: String -> Int -> String

notGroup group = filter (not.(`elem` group)) allASCIICharacters
generateRange (f:(m:l)) = map chr [ord f .. ord (head l)]
duplicate str n = concat $ replicate n str

-- Adding methods --

addAnyToSet                       :: [String] -> [String]
addToSet                          :: [String] -> String -> [String]
addORGroupToSet                   :: [String] -> String -> [String]
addNotORGroupToSet                :: [String] -> String -> [String]
addORRangeGroupToSet              :: [String] -> String -> [String]
addNotORRangeGroupToSet           :: [String] -> String -> [String]
addOptionalToSet                  :: [String] -> String -> [String]

addRepetitionsToSet               :: [String] -> String -> Int -> [String]
addRangeRepetitionsToSet          :: [String] -> String -> (Int, Int) -> [String]
addMultipleRangeRepetitionsToSet  :: [String] -> String -> (Int, Int) -> [String]

addAnyToSet res = concatMap (\c -> map (\w -> w ++[c]) res) allASCIICharacters
addToSet res toAdd = map (++toAdd) res
addOptionalToSet res toAdd = res ++ addToSet res toAdd
addORGroupToSet res = concatMap (\c -> map (\w -> w ++[c]) res)
addNotORGroupToSet res group =
  concatMap (\c -> map (\w -> w ++[c]) res) (notGroup group)
addORRangeGroupToSet res range =
  concatMap (\c -> map (\w -> w ++[c]) res) (generateRange range)
addNotORRangeGroupToSet res range = addNotORGroupToSet res (generateRange range)

addRepetitionsToSet res toAdd repeat = addToSet res (duplicate toAdd repeat)
addRangeRepetitionsToSet res toAdd (n, m) =
  concatMap (addRepetitionsToSet res toAdd) [n..m]
addMultipleRangeRepetitionsToSet res toAdd (0, 0) = res
addMultipleRangeRepetitionsToSet res toAdd (0, m) =
  res ++ addMultipleRangeRepetitionsToSet res toAdd (1, m)
addMultipleRangeRepetitionsToSet res toAdd (n, m) =
  concatMap (\c ->addMultipleRangeRepetitionsToSet
    (addToSet res [c]) toAdd (n-1, m-1)) toAdd
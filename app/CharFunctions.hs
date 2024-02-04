module CharFunctions(allASCIICharacters
                    ,allAlphaNum
                    ,letters
                    ,digits
                    ,whiteSpaces) where

import Data.Char (chr
                 ,isAlphaNum
                 ,isAlpha
                 ,isDigit
                 ,isSpace)

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
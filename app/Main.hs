module Main(main) where

import Regex (generateSetFromRegex)
import SetOperations (isSetOfWordsUniqulyDecodable)
import Data.Set (Set, fromList)

main :: IO ()
main = do
    putStrLn "Podaj regex: "
    line <- getLine
    let generatedSet = fromList (generateSetFromRegex [""] line)
    print generatedSet
    print (isSetOfWordsUniqulyDecodable generatedSet)

module SetOperations(isSetOfWordsUniqulyDecodable) where
-- https://en.wikipedia.org/wiki/Sardinas%E2%80%93Patterson_algorithm

import Data.Set (Set,toList, fromList, map, union, unions, filter, elems, insert, intersection, delete)
import qualified Data.Set as Set

allPostfixes :: String -> Set String
allPostfixes str = fromList [suffix i | i <- [0..length str - 1]]
  where
    suffix :: Int -> String
    suffix start = drop start str

allSetPostfixes :: Set String -> Set String
allSetPostfixes set = unions (Set.map allPostfixes set)

nD :: Set String -> Set String -> Set String
nD nSet dSet = do
  let allDpostfixes = allSetPostfixes dSet
  let f en = Set.filter (\pfD -> en++pfD `elem` dSet) allDpostfixes
  unions (Set.map f nSet)


s :: Set String -> [Set String] -> Bool
s setOfWords [] = do
    let s1 = delete "" (nD setOfWords setOfWords)
    if (setOfWords `intersection` s1) /= Set.empty
        then False
    else s setOfWords [s1]

s setOfWords alreadyGenerated = do
    let si = last alreadyGenerated
    let nextS = nD setOfWords si `union` nD si setOfWords

    if "" `elem` nextS || (nextS `intersection` setOfWords) /= Set.empty
        then False
    else if nextS `elem` alreadyGenerated
        then True
    else s setOfWords (alreadyGenerated ++ [nextS])

isSetOfWordsUniqulyDecodable :: Set String -> Bool
isSetOfWordsUniqulyDecodable wordsSet = s wordsSet []

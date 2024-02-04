import Regex(generateSetFromRegex)

main :: IO ()
main = do
  let testJustLetters = generateSetFromRegex [""] "abc" == ["abc"]
  let testRange = generateSetFromRegex [""] "[1-5]" == ["1", "2", "3", "4", "5"]
  let testOneRep = generateSetFromRegex [""] "a{4}" == ["aaaa"]
  let testOneRepRange = generateSetFromRegex [""] "a{2-4}" == ["aa", "aaa", "aaaa"]
  let testRangeRepRange = generateSetFromRegex [""] "[a-b]{1-3}" == ["a","aa","aaa","aab","ab","aba","abb","b","ba","baa","bab","bb","bba","bbb"]
  print (testJustLetters && testRange && testOneRep && testOneRepRange && testRangeRepRange)

  let notABC = generateSetFromRegex [""] "[^abc]"
  let notABCRange = generateSetFromRegex [""] "[^a-c]"

  let notGroupsTest = notABC == notABCRange && notElem "a" notABC && notElem "b" notABCRange
  let testPipe = generateSetFromRegex [""] "(ab|bc|cd)" == ["cd", "bc", "ab"]
  let testNestedPipe = generateSetFromRegex [""] "(ab|(bc|cd))" == ["ab", "cd", "bc"]
  let optionalTest = generateSetFromRegex [""] "a?" == ["", "a"]
  print (notGroupsTest && testPipe && testNestedPipe && optionalTest)

  let mix = generateSetFromRegex [""] "[ab]{1-2}\\d" == ["a0","aa0","ab0","b0","ba0","bb0","a1","aa1","ab1","b1","ba1","bb1","a2","aa2","ab2","b2","ba2","bb2","a3","aa3","ab3","b3","ba3","bb3","a4","aa4","ab4","b4","ba4","bb4","a5","aa5","ab5","b5","ba5","bb5","a6","aa6","ab6","b6","ba6","bb6","a7","aa7","ab7","b7","ba7","bb7","a8","aa8","ab8","b8","ba8","bb8","a9","aa9","ab9","b9","ba9","bb9"]
  print mix
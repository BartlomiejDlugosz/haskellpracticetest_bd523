module Practice where

isPrefix :: String -> String -> Bool
isPrefix "" _ = True
isPrefix _ "" = False
isPrefix (p:ps) (s:ss) = p == s && isPrefix ps ss

removePrefix :: String -> String -> String
--Pre:
removePrefix "" ss = ss
removePrefix (p:ps) (s:ss) = removePrefix ps ss

suffixes :: [a] -> [[a]]
suffixes [] = []
suffixes str@(_:ss) = str : suffixes ss

isSubstring :: String -> String -> Bool
isSubstring sub s = any (isPrefix sub) (suffixes s)

findSubstrings :: String -> String -> [Int]
findSubstrings sub s = [i | i <- [0..length (suffixes s) - 1], isPrefix sub ((suffixes s)!!i)]

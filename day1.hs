import Data.Char

part1 :: IO ()
part1 = do
  doc <- readFile "day1data"
  print $ numFromDocument doc

part2 :: IO ()
part2 = do
  doc <- readFile "day1data"
  print $ numFromDocument $ r doc

replacements = [("one", "1"),
  ("two", "2"),
  ("three", "3"),
  ("four", "4"),
  ("five", "5"),
  ("six", "6"),
  ("seven", "7"),
  ("eight", "8"),
  ("nine", "9")]

r :: String -> String
r [] = []
r s = [head s'] ++ (r $ drop 1 s')
  where s' = replaceDigits s

replaceDigits = replace replacements

replace :: [(String, String)] -> String -> String
replace [] s = s
replace _ [] = []
replace ((needle, repl):rest) s = case replace' needle repl s of
  Just s' -> s'
  Nothing -> replace rest s

replace' :: String -> String -> String -> Maybe String
replace' needle replacement haystack = if take len haystack == needle
  then Just $ replacement ++ drop len haystack
  else Nothing
    where len = length needle

digitFrom :: ([Char] -> Char) ->  String -> Int
digitFrom from str = read $ [from (filter isDigit str)]

numFromLine :: String -> Int
numFromLine str = (digitFrom head str) * 10 + (digitFrom last str)

numFromDocument :: String -> Int
numFromDocument str = sum $ map numFromLine $ lines str

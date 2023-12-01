import Data.Char

part1 :: IO ()
part1 = do
  doc <- readFile "day1data"
  print $ numFromDocument doc

part2 :: IO ()
part2 = do
  doc <- readFile "day1data"
  print $ numFromDocument $ replaceWords doc

replacements = [("one", "1"),
  ("two", "2"),
  ("three", "3"),
  ("four", "4"),
  ("five", "5"),
  ("six", "6"),
  ("seven", "7"),
  ("eight", "8"),
  ("nine", "9")]

replaceWords :: String -> String
replaceWords [] = []
replaceWords s = [head s'] ++ (replaceWords $ drop 1 s')
  where s' = replaceStartIfMatch replacements s

replaceStartIfMatch :: [(String, String)] -> String -> String
replaceStartIfMatch [] s = s
replaceStartIfMatch _ [] = []
replaceStartIfMatch ((needle, repl):rest) haystack = if take len haystack == needle
  then repl ++ drop len haystack
  else replaceStartIfMatch rest haystack
    where len = length needle

digitFrom :: ([Char] -> Char) ->  String -> Int
digitFrom from str = read $ [from (filter isDigit str)]

numFromLine :: String -> Int
numFromLine str = (digitFrom head str) * 10 + (digitFrom last str)

numFromDocument :: String -> Int
numFromDocument str = sum $ map numFromLine $ lines str

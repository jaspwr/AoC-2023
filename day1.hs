import Data.Char
import Data.Maybe (fromMaybe)

part1 :: IO ()
part1 = do
  doc <- readFile "day1data"
  print $ numFromDocument numFromLine doc

part2 :: IO ()
part2 = do
  doc <- readFile "day1data"
  print $ numFromDocument lineWithWords doc

digitFrom :: ([Char] -> Char) -> String -> Int
digitFrom from str = if length digits == 0
  then 0
  else read $ [from digits]
    where digits = filter isDigit str

numFromLine :: String -> Int
numFromLine str = (digitFrom head str) * 10 + (digitFrom last str)

numFromDocument :: (String -> Int) -> String -> Int
numFromDocument fn str = sum $ map fn $ lines str

lineWithWords s = (fromMaybe 0 (firstDigit s)) * 10 + (fromMaybe 0 (lastDigit s))

matches = [("one", 1), ("1", 1),
  ("two", 2), ("2", 2),
  ("three", 3), ("3", 3),
  ("four", 4), ("4", 4),
  ("five", 5), ("5", 5),
  ("six", 6), ("6", 6),
  ("seven", 7), ("7", 7),
  ("eight", 8), ("8", 8),
  ("nine", 9), ("9", 9)] :: [(String, Int)]

firstDigit [] = Nothing
firstDigit s = case tryAllMatches s of
  Just val -> Just val
  Nothing -> firstDigit $ drop 1 s

lastDigit s = lastDigit' s $ length s

lastDigit' _ (-1) = Nothing
lastDigit' s ptr = case tryAllMatches (drop ptr s) of
  Just val -> Just val
  Nothing -> lastDigit' s $ ptr - 1

tryAllMatches :: String -> Maybe Int
tryAllMatches = tryAllMatches' matches

tryAllMatches' :: [(String, Int)] -> String -> Maybe Int
tryAllMatches' [] _ = Nothing
tryAllMatches' ((needle, value):rest) haystack = if take len haystack == needle
  then Just value
  else tryAllMatches' rest haystack
    where len = length needle

import Data.List (isInfixOf)

main :: IO ()
main = do
  dat <- readFile "day4data"
  let l = lines dat
  print $ part1 l
  print $ part2 l

part1 :: [String] -> Int
part1 l = sum $ map (scoreOfCount . correctCount) l

part2 :: [String] -> Int
part2 l = sum [copiesCount $ drop n counts | n <- [0..(len-1)]]
  where
    counts = map correctCount l
    len = length l

copiesCount :: [Int] -> Int
copiesCount [] = 0
copiesCount counts = 1 + sum ([copiesCount $ drop c' counts | c' <- [1 .. correct]])
  where
    correct = head counts

scoreOfCount :: Int -> Int
scoreOfCount count = if count == 0 then 0 else 2 ^ (count - 1)

correctCount :: String -> Int
correctCount s = length $ filter (\n -> [n] `isInfixOf` correct) elfsNums
  where
    colonSpl = splitByChar ':' s
    pipeSpl = splitByChar '|' $ colonSpl !! 1
    correct = parseNumList $ head pipeSpl
    elfsNums = parseNumList $ pipeSpl !! 1

parseNumList :: String -> [Int]
parseNumList s = map read $ filter (/= "") $ splitByChar ' ' s

splitByChar :: Char -> String -> [String]
splitByChar spl (c : cs) =
  if c == spl
    then [] : (n : ns)
    else (c : n) : ns
  where
    (n : ns) = splitByChar spl cs
splitByChar _ c = [c]

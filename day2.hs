data Game = Game {
  game_id :: Int,
  draws :: [Draw]}
    deriving (Read, Show, Eq)

data Draw = Draw {
  r :: Int,
  g :: Int,
  b :: Int}
    deriving (Read, Show, Eq)

main :: IO ()
main = do
  dat <- readFile "day2data"
  print $ part1 dat
  print $ part2 dat

part1 :: String -> Int
part1 s = sum $ map
  (\game -> if isGamePossible game elfLimits then game_id game else 0) games
  where
    games = parseGames s

elfLimits = Draw 12 13 14

part2 :: String -> Int
part2 s = sum $ map setPower games
  where
    games = parseGames s

isGamePossible :: Game -> Draw -> Bool
isGamePossible game limits = (r limits >= max_r)
  && (g limits >= max_g)
  && (b limits >= max_b)
    where (max_r, max_g, max_b) = gameMaxes game

setPower :: Game -> Int
setPower game = max_r * max_g * max_b
  where (max_r, max_g, max_b) = gameMaxes game

gameMaxes :: Game -> (Int, Int, Int)
gameMaxes game = (max_r, max_g, max_b)
    where
      max_r = maxOf r game
      max_g = maxOf g game
      max_b = maxOf b game
      maxOf :: (Draw -> Int) -> Game -> Int
      maxOf col ds = maximum $ map col $ draws ds

parseGames :: String -> [Game]
parseGames = map parseGame . lines

parseGame :: String -> Game
parseGame s = Game game_id $ map parseDraw draws
  where
    colon_spl = splitByChar ':' s
    game_id = read $ splitByChar ' ' (head colon_spl) !!1
    draws = splitByChar ';' $ colon_spl!!1

parseDraw :: String -> Draw
parseDraw s = parseDraw' comma_spl
  where comma_spl = splitByChar ',' s

parseDraw' :: [String] -> Draw
parseDraw' [] = Draw 0 0 0
parseDraw' (s:cs)
  | head col_name == 'r' = Draw val (g next) (b next)
  | head col_name == 'g' = Draw (r next) val (b next)
  | head col_name == 'b' = Draw (r next) (g next) val
    where
      space_spl = splitByChar ' ' s
      col_name = space_spl!!2
      val = read $ space_spl!!1
      next = parseDraw' cs

splitByChar :: Char -> String -> [String]
splitByChar spl (c:cs) = if c == spl
  then [] : (n:ns)
  else (c:n) : ns
    where (n:ns) = splitByChar spl cs
splitByChar _ c = [c]

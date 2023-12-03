import Data.Bifunctor qualified
import Data.Char (isDigit)
import Data.List (isInfixOf)
import Data.Maybe (mapMaybe)
import Data.Set (fromList, toList)

main :: IO ()
main = do
  dat <- readFile "day3data"
  let schem = lines dat
  print "Part 1"
  print $ part1 schem
  print "Part 2"
  print $ part2 schem
  return ()

type Schem = [String]

part1 :: Schem -> Int
part1 = sum . numsInSchem

part2 :: Schem -> Int
part2 schem = sum $ map ratio validGears
  where
    ratio [n1, n2] = read n1 * read n2
    validGears = filter ((== 2) . length) gearNeighbors
    gearNeighbors = map (getNeighbouringWords schem) $ gearPositions schem

neighbourOffsets =
  [ (1, 1),
    (1, 0),
    (1, -1),
    (0, 1),
    (0, -1),
    (-1, -1),
    (-1, 0),
    (-1, 1)
  ] :: [(Int, Int)]

numsInSchem :: Schem -> [Int]
numsInSchem schem = concat [numsInRow y schem | y <- [0 .. (h - 1)]]
  where
    (_, h) = dims schem

numsInRow :: Int -> Schem -> [Int]
numsInRow y schem =
  map
    (read . fst)
    $ filter hasAdjacentSymbol'
    $ fst
    $ numsInRow' (w - 1, y) schem
  where
    hasAdjacentSymbol' = hasAdjacentSymbol schem
    (w, _) = dims schem

type Pos = (Int, Int)

type Word' = (String, Pos)

type IsInWord = Bool

numsInRow' :: Pos -> Schem -> ([Word'], IsInWord)
numsInRow' (-1, _) _ = ([], False)
numsInRow' (x, y) schem = (words, is_in_word)
  where
    words
      | is_in_word && was_in_word =
          (fst (head next) ++ [c], (x, y)) : drop 1 next
      | is_in_word && not was_in_word = ([c], (x, y)) : next
      | otherwise = next

    c = schem !! y !! x
    is_in_word = isDigit c
    (next, was_in_word) = numsInRow' (x - 1, y) schem

hasAdjacentSymbol :: Schem -> Word' -> Bool
hasAdjacentSymbol schem (s, (x, y)) =
  containsSymbols $
    concat [getNeighbours (x - x', y) schem | x' <- [0 .. len]]
  where
    len = length s - 1

containsSymbols :: String -> Bool
containsSymbols = any $ \c -> not (isDigit c) && c /= '.'

getNeighbours :: Pos -> Schem -> [Char]
getNeighbours (x_center, y_center) schem = mapMaybe f neighbourOffsets
  where
    f (dx, dy) = charAtPos schem (pos dx dy)
    pos dx dy = (x_center + dx, y_center + dy)

charAtPos :: Schem -> Pos -> Maybe Char
charAtPos schem (x, y) =
  if not $ isLegalPos dims' (x, y)
    then Nothing
    else Just $ schem !! y !! x
  where
    dims' = dims schem

isLegalPos :: Pos -> Pos -> Bool
isLegalPos (w, h) (x, y) = not $ x < 0 || x >= w || y < 0 || y >= h

dims :: Schem -> Pos
dims schem = (length $ head schem, length schem)

gearPositions :: Schem -> [Pos]
gearPositions schem = filter isGear poses
  where
    isGear (x, y) = schem !! y !! x == '*'
    poses = concat [[(x, y) | x <- [0 .. (w - 1)]] | y <- [0 .. (h - 1)]]
    (w, h) = dims schem

unwrap :: Maybe a -> a
unwrap (Just x) = x

wordAt :: Schem -> Pos -> String
wordAt schem (x, y) = case charAtPos schem (x, y) of
  Just c ->
    if isDigit c
      then c : wordAt schem (x + 1, y)
      else ""
  Nothing -> ""

getNeighbouringWords :: Schem -> Pos -> [String]
getNeighbouringWords schem (x, y) =
  map (wordAt schem) $
    removeDups $
      map (wordStartsAt schem) poses_with_digits
  where
    poses_with_digits = filter (isDigit . unwrap . charAtPos schem) poses
    poses =
      filter (isLegalPos dims') $
        map
          (Data.Bifunctor.bimap (x +) (y +))
          neighbourOffsets
    dims' = dims schem

-- Courtesy of stack overflow.
removeDups :: (Ord a) => [a] -> [a]
removeDups = toList . fromList

wordStartsAt :: Schem -> Pos -> Pos
wordStartsAt schem (x, y) = case charAtPos schem (x, y) of
  Nothing -> (x + 1, y)
  Just c ->
    if isDigit c
      then prev_pos
      else (x + 1, y)
  where
    prev_pos = wordStartsAt schem (x - 1, y)

import Distribution.Simple.Utils (xargs, isInfixOf)
import Data.List (intersect)
main = do
  ranges <- readInputFile "input2.txt"
  let result = concatMap (filter (not . isValid)) ranges
  print $ sum result


readInputFile fileName = do
  contents <- readFile fileName
  let x = split ',' contents
  let y = map (split '-') x
  let z = map (\a -> [(read (head a) :: Integer) .. (read (last a) :: Integer)]) y
  return z


split :: Char -> String -> [String]
split delim str = case break (== delim) str of
    (a, "")       -> [a]
    (a, _:b)      -> a : split delim b

isValid :: Integer -> Bool
isValid id = not $ hasAtLeastTwoRepeatingSequences $ show id

cutInHalfAndSwap :: String -> String
cutInHalfAndSwap x =
  let i = length x `div` 2
      a = take i x
      b = reverse $ take i $ reverse x
      res = b ++ a
  in res

hasAtLeastTwoRepeatingSequences :: String -> Bool
hasAtLeastTwoRepeatingSequences x =
  let combinations = generateCombinations x
      stringsToCheck = foldl (\acc y -> if rem (length x) (length y) == 0 then acc ++ [concat (replicate (length x `div` length y) y)]  else acc ) [] combinations
  in elem x stringsToCheck

-- generateCombinations :: String -> [String]
-- generateCombinations x
--   | null x = []
--   | otherwise = map (`take` x) [1..length x] ++ generateCombinations (tail x)

generateCombinations :: String -> [String]
generateCombinations x = map (`take` x) [1..length x - 1]

getSequences :: Int -> String -> [String]
getSequences len x
  | length x == len = [x]
  | otherwise = take len x : getSequences len (tail x)

determineStreak :: String -> [String] -> Int
determineStreak x combinations = fst $ foldl (\acc y -> if y == x then (fst acc + 1, length x) else if snd acc > 0 then (fst acc, snd acc - 1) else (0,0) ) (0,0) combinations

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

hasAtLeastTwoRepeatingSequences :: String -> Bool
hasAtLeastTwoRepeatingSequences x =
  let combinations = generateCombinations x
      stringsToCheck = foldl (\acc y -> if rem (length x) (length y) == 0 then acc ++ [concat (replicate (length x `div` length y) y)]  else acc ) [] combinations
  in elem x stringsToCheck

generateCombinations :: String -> [String]
generateCombinations x = map (`take` x) [1..length x - 1]

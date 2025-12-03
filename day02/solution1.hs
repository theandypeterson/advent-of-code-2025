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
isValid id
  | odd $ length x = True
  | otherwise = x /= cutInHalfAndSwap x
  where x = show id

cutInHalfAndSwap :: String -> String
cutInHalfAndSwap x =
  let i = length x `div` 2
      a = take i x
      b = reverse $ take i $ reverse x
      res = b ++ a
  in res
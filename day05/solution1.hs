import Data.List (isInfixOf)
main = do
  input <- readInputFile "input2.txt"
  let result = findFreshIngredients input
  print $ length result

readInputFile fileName = do
  contents <- readFile fileName
  let raw = lines contents
  let dataset :: DataSet = foldl buildDataSet ([], []) raw
  return dataset

buildDataSet :: DataSet -> String -> DataSet
buildDataSet (numbers, ranges) l
  | "-" `isInfixOf` l = (numbers, ranges ++ [(first,second)])
  | "" == l = (numbers, ranges)
  | otherwise = (numbers ++ [read l], ranges)
  where x = split '-' l
        first :: Int = read (head x)
        second :: Int = read (last x)

type DataSet = ([Int], [Range])
type Range = (Int, Int)

split :: Char -> String -> [String]
split delim str = case break (== delim) str of
    (a, "")       -> [a]
    (a, _:b)      -> a : split delim b

isInRange :: Range -> Int -> Bool
isInRange (first, second) i = i >= first && i <= second

isInAnyRange :: [Range] -> Int -> Bool
isInAnyRange ranges i = any (`isInRange` i) ranges

findFreshIngredients :: DataSet -> [Int]
findFreshIngredients (ingredients, ranges) = filter (isInAnyRange ranges) ingredients

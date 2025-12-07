import Data.List (isInfixOf, sortBy)
main = do
  (_, ranges) <- readInputFile "input2.txt"
  let sortedRanges = sortBy compareRange ranges
  consolidatedRanges <- consolidateAllRanges 0 sortedRanges
  -- print ranges
  -- print sortedRanges
  print consolidatedRanges
  print $ findFreshIngredientsCount consolidatedRanges

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


findFreshIngredientsCount :: [Range] -> Int
findFreshIngredientsCount ranges = sum $ map (\(x, y) -> y-x+1) ranges

rangesOverlap :: Range -> Range -> Bool
rangesOverlap (x1, y1) (x2, y2) = (x1 >= x2 && x1 <= y2) || (y1 >= x2 && y1 <= y2) || (x1 < x2 && y1 > y2)

compareRange :: Range -> Range -> Ordering
compareRange (x1, _) (x2, _) = if x1 < x2 then LT else GT

consolidateRanges :: Range -> Range -> [Range]
consolidateRanges r1 r2 = if rangesOverlap r1 r2 then [(min (fst r1) (fst r2), max (snd r1) (snd r2))] else [r1, r2]

consolidateAllRanges :: Int -> [Range] -> IO [Range]
consolidateAllRanges index ranges
  | length ranges <= 1  = return ranges
  | length ranges == 2  = return (consolidateRanges (head ranges) (last ranges))
  | length ranges <= index+1 = return ranges
  | otherwise =
    do
      let currentRange = ranges !! index
      let nextRange = ranges !! (index + 1)
      -- print currentRange
      -- print nextRange
      let consolidated = consolidateRanges currentRange nextRange
      let prevRanges = take index ranges
      let nextRanges = drop (index + 2) ranges
      let newRanges = prevRanges  ++ consolidated ++ nextRanges
      -- print prevRanges
      -- print consolidated
      -- print nextRanges
      let newIndex = if length consolidated == 1 then index else index + 1
      -- print newIndex
    -- in consolidateAllRanges newIndex newRanges
      consolidateAllRanges newIndex newRanges

main = do
  banks <- readInputFile "input2.txt"
  let result = sum $ map determineMaxJoltage banks
  print result


readInputFile fileName = do
  contents <- readFile fileName
  let rawStrings = lines contents
  let banks = map (\l -> [read [x] :: Int | x <- l]) rawStrings
  return banks

generateCombinations :: [Int] -> [(Int, Int)]
generateCombinations list
  | length list == 2 = [(head list, last list)]
  | otherwise = map (\d -> (head list, d)) (tail list)  ++ generateCombinations (tail list)

calcJoltage :: (Int, Int) -> Int
calcJoltage (x, y) = (x*10) + y

determineMaxJoltage :: [Int] -> Int
determineMaxJoltage bank = maximum $ map calcJoltage (generateCombinations bank)
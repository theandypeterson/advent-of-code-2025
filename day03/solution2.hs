main = do
  banks <- readInputFile "input1.txt"
  let result = map determineMaxJoltage banks
  print result
  print $ sum result


readInputFile fileName = do
  contents <- readFile fileName
  let rawStrings = lines contents
  let banks = map (\l -> [read [x] :: Int | x <- l]) rawStrings
  return banks

generateCombinations :: Int -> [Int] -> [[Int]]
generateCombinations len list
  | len == 1 = map (: []) list
  | len == length list = [list]
  | otherwise =
    let currNum = head list
        prevCombos = generateCombinations (len-1) (tail list)
        res = map (currNum :) prevCombos
    in res ++ [tail list]

calcJoltage :: [Int] -> Int
calcJoltage l = read (concatMap show l) :: Int

determineMaxJoltage :: [Int] -> Int
determineMaxJoltage bank = maximum $ map calcJoltage (generateCombinations 12 bank)
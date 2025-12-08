import Data.List (transpose)
main = do
  input <- readInputFile "input2.txt"
  let rawNumbers = init input
  let operations = last input
  let (leftover, operationWithIndex) = foldl buildOperationWithIndex (0,[]) operations
  let spaces = zipWith (\ x i
        -> (if i + 1 == length operationWithIndex then
                leftover - fst x
            else
                fst (operationWithIndex !! (i + 1)) - fst x - 1))  operationWithIndex [0..]
  let tranposedNumbers = map transpose $ transpose $ map (getNumbersFromString spaces) rawNumbers
  let numbers = map (map convertToNumber) tranposedNumbers
  let zipped = zip numbers (map snd operationWithIndex)
  let result = map applyOperation zipped
  print $ sum result

readInputFile :: FilePath -> IO [String]
readInputFile fileName = do
  contents <- readFile fileName
  let raw = lines contents
  return raw

applyOperation :: ([Int], Char) -> Int
applyOperation (numbers, operation)
  | operation == '+' = sum numbers
  | otherwise = product numbers

buildOperationWithIndex :: (Int, [(Int, Char)]) -> Char -> (Int, [(Int, Char)])
buildOperationWithIndex (i, l) ch = if ch == ' ' then (i+1, l) else (i+1, l ++ [(i, ch)])

getNumbersFromString :: [Int] -> String -> [String]
getNumbersFromString spaces raw = fst (foldl (\(res, r) x -> (res ++ [take x r], drop (x+1) r) ) ([], raw) spaces)

fillZeros :: String -> String
fillZeros = map (\c -> if c == ' ' then '0' else c)

convertToNumber :: String -> Int
convertToNumber = read

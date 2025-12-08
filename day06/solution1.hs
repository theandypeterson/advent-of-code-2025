import Data.List (transpose)
main = do
  input <- readInputFile "input2.txt"
  let rawNumbers = init input
  let numbers = transpose (map (\x -> map read x :: [Int]) rawNumbers)
  let operations = last input
  let zipped = zip numbers operations
  print $ sum $ map applyOperation zipped

readInputFile :: FilePath -> IO [[String]]
readInputFile fileName = do
  contents <- readFile fileName
  let raw = map words $ lines contents
  return raw

applyOperation :: ([Int], String) -> Int
applyOperation (numbers, operation)
  | operation == "+" = sum numbers
  | otherwise = product numbers
main = do
  instructions <- readInputFile "input2.txt"
  let result = length $ filter (==0) $ foldInstructions 50 instructions
  print result


readInputFile fileName = do
  contents <- readFile fileName
  let instructions = map parseInstruction $ lines contents
  return instructions

type Instruction = (Char, Int)

parseInstruction :: String -> Instruction
parseInstruction line =
  let direction = head line
      num = read $ tail line :: Int
  in (direction, num)

applyInstruction ::  Int -> Instruction -> Int
applyInstruction position instruction
  | direction == 'L' = (position - num) `mod` 100
  | otherwise =  (position + num) `mod` 100
  where num = snd instruction
        direction = fst instruction

foldInstructions :: Int -> [Instruction] -> [Int]
foldInstructions pos instructions
  | null instructions = []
  | otherwise = currPos : foldInstructions currPos (tail instructions)
  where currPos =applyInstruction pos (head instructions)



main = do
  instructions <- readInputFile "input2.txt"
  let x =  foldInstructions 50 instructions
  let result = sum $ map snd x
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

applyInstruction ::  Int -> Instruction -> (Int, Int)
applyInstruction position instruction
  | direction == 'L' = ((position - num) `mod` 100, determineClicks (position==0) (position - num))
  | otherwise =  ((position + num) `mod` 100, determineClicks (position==0) ( position + num))
  where num = snd instruction
        direction = fst instruction

foldInstructions :: Int -> [Instruction] -> [(Int, Int)]
foldInstructions pos instructions
  | null instructions = []
  | otherwise = currPos : foldInstructions (fst currPos) (tail instructions)
  where currPos =applyInstruction pos (head instructions)

determineClicks :: Bool -> Int -> Int
determineClicks wasZero pos =
  if pos > 0 then div pos 100 else offset + div (negate pos) 100
  where offset = if wasZero then 0 else  1


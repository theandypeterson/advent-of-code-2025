main = do
  input <- readInputFile "input2.txt"
  let coords :: [Coordinates] = concatMap (\y -> map (\x -> (x,y)) [0..length $ head input]) [0..length input]
  let result = length $ filter (canGetRoll input) coords
  print result

readInputFile fileName = do
  contents <- readFile fileName
  let mapOfRolls = lines contents
  return mapOfRolls


type Map = [String]
type Coordinates = (Int, Int)

canGetRoll :: Map -> Coordinates -> Bool
canGetRoll m (x, y) =
  let coordOffsets = [(1,1), (1,0), (1,-1), (0, 1), (0,-1), (-1,1), (-1,0), (-1,-1)]
      surroundings = map (\(x1, y1) -> getThing m (x+x1,y+y1) ) coordOffsets
   in length (filter (== '@') surroundings) < 4 && getThing m (x,y) == '@'

getThing :: Map -> Coordinates -> Char
getThing m (x, y)
  | x < 0 || y < 0 = '.'
  | x >= length (head m) || y >= length m = '.'
  | otherwise = (m !! y) !! x
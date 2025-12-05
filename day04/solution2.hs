import Data.List (unfoldr)
main = do
  input <- readInputFile "input2.txt"
  result <- theLoop 0 1 input
  -- let result = getCoords input
  print result

readInputFile fileName = do
  contents <- readFile fileName
  let rawMap = lines contents
  return (concat rawMap, length $ head rawMap, length rawMap )


type Map = (String, Int, Int)
type Coordinates = (Int, Int)

canGetRoll :: Map -> Coordinates -> Bool
canGetRoll m (x, y) =
  let coordOffsets = [(1,1), (1,0), (1,-1), (0, 1), (0,-1), (-1,1), (-1,0), (-1,-1)]
      surroundings = map (\(x1, y1) -> getThing m (x+x1,y+y1) ) coordOffsets
   in length (filter (== '@') surroundings) < 4 && getThing m (x,y) == '@'

getThing :: Map -> Coordinates -> Char
getThing (m, width, height) (x, y)
  | x < 0 || y < 0 = '.'
  | x >= width || y >= height = '.'
  | otherwise = m !! (x + (height * y))

getCoords :: Map -> [Coordinates]
getCoords (_, w, h) =
  [ (x, y) | y <- [0..h-1], x <- [0..w-1] ]

determineRolls :: Map -> Int
determineRolls m = length $ filter (canGetRoll m) (getCoords m)

-- theLoop :: Int -> Int -> Map -> Int
theLoop count lastResult m
  | lastResult == 0 = do
    return count
  | otherwise = do
    let numberOfRolls = determineRolls m
    let nextCount = count + numberOfRolls
    let nextMap = removeRolls m
    -- printMap nextMap
    -- print "------------"
    theLoop nextCount numberOfRolls nextMap


chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

printMap :: (String, Int, Int) -> IO ()
printMap (s, w, h) = do
    let rows = take h (chunksOf w s)
    mapM_ putStrLn rows

removeRolls :: Map -> Map
removeRolls m =
  let coords = getCoords m
      shouldRemove = map (canGetRoll m) coords
      zipped = zip coords shouldRemove
      newMap = map (\(c, s) -> if s then '.' else getThing m c ) zipped
   in (newMap, getWidth m, getHeight m)

getWidth :: Map -> Int
getWidth (_,x,_) = x

getHeight :: Map -> Int
getHeight (_,_,x) = x

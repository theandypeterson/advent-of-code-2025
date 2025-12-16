
import qualified Data.Map as Map
import Data.List (elemIndex)
import Distribution.Compat.Prelude (fromMaybe)
import Data.Maybe (isNothing, isJust)
main = do
  ((layout, width, height), starterCoor) <- readInputFile "input2.txt"
  printLayout (layout, width, height)
  let x = foldl calcRow (layout, width, height) [0..height-1]
  printLayout x
  print starterCoor
  -- let getTimelines = memoize Map.empty $ timelines x
  let getTimelines = timelines Map.empty x
  print $ fst $ getTimelines starterCoor

data NodeType = Empty | Splitter | Starter | Laser deriving (Show, Eq)

type Coordinates = (Int, Int)
type Layout = (Map.Map Coordinates NodeType, Int, Int)
type Cache = (Map.Map Coordinates Int)

readInputFile fileName = do
  contents <- readFile fileName
  let theLines = lines contents
  let starterIndex = elemIndex 'S' (head theLines)
  let raw = zip theLines [0..]
  let numRows = length raw
  let numCols = length $ head theLines
  let res :: Layout = (Map.fromList $ concatMap (\(l, y) -> zipWith (\ c x -> ((x, y), determinNodeType c)) l [0..] ) raw, numCols + 1, numRows)
  return (res, (fromMaybe 0 starterIndex, 0))

determinNodeType :: Char -> NodeType
determinNodeType c
  | c == '.' = Empty
  | c == '^' = Splitter
  | c == 'S' = Starter
  | c == '|' = Laser

calcRow :: Layout -> Int -> Layout
calcRow (layout, width, height) row = foldl calcCell (layout, width, height) [(x,row) | x <- [0..width-1]]

calcCell :: Layout -> Coordinates -> Layout
calcCell (layout, width, height) coor
  | c == Just Empty && (aboveC == Just Laser || aboveC == Just Starter) = (Map.insert coor Laser layout, width, height)
  | c == Just Splitter && aboveC == Just Laser = (Map.insert (fst coor - 1, snd coor) Laser $ Map.insert (fst coor + 1, snd coor) Laser layout, width, height)
  | otherwise = (layout, width, height)
  where c = Map.lookup coor layout
        aboveC = Map.lookup (fst coor, snd coor - 1) layout

countUsedSplitters :: Layout -> Int
countUsedSplitters (layout, width, height) = sum $ concatMap (\y -> map (\x -> if (Map.lookup (x,y) layout == Just Splitter) && (Map.lookup (x,y-1) layout == Just Laser) then 1 else 0) [0..width-1]) [0..height-1]

printLayout :: Layout -> IO ()
printLayout (layout, width, height) = do
  let irow = [0..height-1]
  let icol = [0..width-1]
  let res = map (\y -> map (\x -> getNodeChar $ Map.lookup (x,y) layout ) icol) irow
  putStrLn $ unlines res

getNodeChar :: Maybe NodeType -> Char
getNodeChar nodeType
  | nodeType == Just Empty = '.'
  | nodeType == Just Splitter = '^'
  | nodeType == Just Laser = '|'
  | nodeType == Just Starter = 'S'
  | otherwise = 'x'

timelines :: Cache -> Layout -> Coordinates -> (Int, Cache)
timelines cache lay@(layout, w, h) coor@(x,y) =
  case Map.lookup coor cache of
    Just v -> (v, cache)
    Nothing ->
      let (v, cache2) = compute cache
          cache3 = Map.insert coor v cache2
      in (v, cache3)
      where
        c = Map.lookup coor layout
        compute :: Cache -> (Int, Cache)
        compute c0
          | c == Just Laser || c == Just Starter = timelines c0 (layout, w, h) (x, y+1)
          | c == Just Splitter =
            let (v1, c1) = timelines c0 (layout, w, h) (x-1, y)
                (v2, c2) = timelines c1 (layout, w, h) (x+1, y)
             in (v1 + v2, c2)
          | isNothing c = (1, c0)
  -- where c = Map.lookup (x,y) layout
  --       cached = Map.lookup (x,y) cache

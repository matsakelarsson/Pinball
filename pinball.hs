import Prelude hiding (Left, Right, flip)
import System.IO

data Cell = Empty | Left | Right
  deriving (Show, Eq)

type Grid = [[Cell]]

data Direction = North | South | East | West
  deriving (Show, Eq)

type Border = (Direction, Int)

type Position = (Int, Int)

type Path = [Position]

validGrid :: Grid -> Bool
validGrid [] = error "Grid empty"
validGrid (x:xs)
          |length (x:xs) == rowLength (x:xs) = True
          |otherwise = False

rowLength :: Grid -> Int
rowLength [] = error "Grid empty"
rowLength [x] = length x
rowLength (x:xs)
          |length (x) == rowLength' = length (x)
          |otherwise = rowLength'
          where rowLength' = rowLength xs

validEntryPoint :: Grid -> Border -> Position
validEntryPoint [] (d, y) = error "Grid empty"
validEntryPoint g (North, x) = if 0 <= x && x < length g then (x, 0) else error "Coordinates Outside of Grid"
validEntryPoint g (South, x) = if 0 <= x && x < length g then (x, length g -1) else error "Coordinates Outside of Grid"
validEntryPoint (g:gs) (West, y) = if 0 <= y && y < length g then (0, y) else error "Coordinates Outside of Grid"
validEntryPoint (g:gs) (East, y) = if 0 <= y && y < length g then (length g -1, y) else error "Coordinates Outside of Grid"

trajectory :: Grid -> Border -> Path
trajectory g b = []





--convertPositionToBorder :: Position -> Border
--convertPositionToBorder (x, y)
   --            |x == length



play :: Grid -> Border -> Border
play g b = b
 --     putStrLn "Where do you want to start?"
 --     print g
 --     b <- getLine
  --    return (last (trajectory g b))


-- HUnit Test Cases

-- data for test cases

goodgrid :: Grid
goodgrid = [[Empty, Left,  Empty, Empty, Right],
            [Left,  Right, Empty, Empty, Left ],
            [Empty, Empty, Right, Empty, Right],
            [Right, Empty, Empty, Empty, Empty],
            [Empty, Empty, Empty, Left,  Empty]]

badgrid :: Grid
badgrid = [[Empty, Left,  Empty, Empty, Right],
           [Left,  Right, Empty, Empty, Left ],
           [Empty, Empty, Right, Empty],
           [Right, Empty, Empty, Empty, Empty],
           [Empty, Empty, Empty, Left,  Empty]]

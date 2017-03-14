import Prelude hiding (Left, Right, flip)
import Test.HUnit hiding (Path)

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

validEntryPoint :: Grid -> Border -> Bool
validEntryPoint [] (d, y) = error "Grid empty"
validEntryPoint (x:xs) (North, y) = y == 0
validEntryPoint (x:xs) (South, y) = y == length (x:xs)
validEntryPoint (x:xs) (West, y) = y == 0
validEntryPoint (x:xs) (East, y) = y == length x

trajectory :: Grid -> Border -> Path
trajectory _ _ = []

play :: Grid -> Border -> Border
play _ b = b


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

-- test cases
test0 = TestCase $ assertBool "Valid Grid"
                   (validGrid goodgrid)

test1 = TestCase $ assertBool "Invalid Grid"
                   (not (validGrid badgrid))

test2 = TestCase $ assertBool "Valid Entry Point"
                   (validEntryPoint goodgrid (South, 3))

test3 = TestCase $ assertBool "Invalid Entry Point"
                   (not (validEntryPoint goodgrid (South, 12)))

test4 = TestCase $ assertEqual "Play (South, 3)"
                   (South, 1) (play goodgrid (South, 2))


test5 = TestCase $ assertEqual "Trajectory (South, 2)" traj (trajectory goodgrid (South, 2))
  where traj = [(2, 4), (2, 3), (2, 2), (3, 2), (4, 2), (4, 1),
                (3, 1), (2, 1), (1, 1), (1, 2), (1, 3), (1, 4)]

runTests = runTestTT $ TestList [test0, test1, test2, test3, test4, test5]

extraTests = rutTestTT $ TestList [] -- Add your tests to this list

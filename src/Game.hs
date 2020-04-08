module Game where
import Data.Array
import Data.Tuple
import Data.Maybe
import System.Random
import System.Random.Shuffle

data Winning = Winning|Losing  deriving (Eq, Show)
data Player = PlayerX | PlayerO | PlayerY | PlayerW deriving (Eq, Show)
data Shape = Square | LTriangle | RTriangle | Window deriving (Eq,Show)
type Cell = Maybe Shape
data State = Running | GameOver (Winning) deriving (Eq, Show)

type Board = Array (Int, Int) Cell
type Solution = [Maybe Shape]
type Solutionlist = [Solution]

data Game = Game { gameBoard :: Board
                 , gameShape :: Shape
                 , gameState :: State
                 , shapeList :: [Shape]
                 , gameSolution :: Board
                 , solutionShapes :: Solution
                 , nextSolutions :: Solutionlist
                 } deriving (Eq, Show)

n :: Int
n = 5

shapeList1 = concat $ repeat $ [Square,LTriangle, Square,RTriangle, Square,Window,Square]
screenWidth :: Int
screenWidth = 640

screenHeight :: Int
screenHeight = 640

cellWidth :: Float
cellWidth = fromIntegral screenWidth / fromIntegral n

cellHeight :: Float
cellHeight = fromIntegral screenHeight / fromIntegral n

smallPicWidth :: Float
smallPicWidth = 0.25*cellWidth

smallPicHeight :: Float
smallPicHeight = 0.25 * cellHeight

initialGame solutionshapeslist = Game { gameBoard = array indexRange $ zip (range indexRange) (repeat Nothing)
                   , gameShape = head shapelist
                   , gameState = Running
                   , shapeList = tail shapelist
                   , gameSolution = (solution solutionshapes)
                   , solutionShapes = solutionshapes
                   , nextSolutions = nextsolutions
                   }
    where indexRange = ((0, 0), (n - 1, n - 1))
          solutionshapes = head solutionshapeslist
          nextsolutions =  tail solutionshapeslist
          shapelist = concat $repeat$solution2shapelist randomGenStd solutionshapes

transposed :: [(a,b)]->[(b,a)]
transposed tuplelist = map swap tuplelist

solution playerlist = array indexRange $ zip (transposed (range indexRange))(playerlist ++ repeat Nothing)
    where indexRange = ((0, 0), (n - 1, n - 1))

randomGenStd = mkStdGen 42 

extractfrommaybes =  map (fromJust) . (filter isJust)

solution2shapelist randomgen playerlist = shuffle' shapeslist (length shapeslist) randomgen

  where shapeslist = extractfrommaybes playerlist
--suggestions for building

playerlist1 = [Just Square, Just Square, Just LTriangle, Nothing, Nothing,Just Square, Just Square, Just Window, Just LTriangle, Nothing,Just Square, Just Square, Just Window, Just RTriangle, Nothing,Just Square, Just Square, Just RTriangle,Nothing,Nothing]

playerlist2 = [Just Square, Just Square, Just LTriangle, Nothing, Nothing,Just Square, Just Square, Just RTriangle, Nothing, Nothing, Just Square, Just Window, Just LTriangle, Nothing, Nothing, Just Square, Just Square, Just RTriangle, Nothing,Nothing]

playerlist3 = [Just Square, Just LTriangle, Nothing, Nothing, Nothing,Just Square, Just RTriangle, Nothing, Nothing, Nothing, Just Square, Just Square, Just Square, Just LTriangle, Nothing, Just Square, Just Square,Just Window, Just RTriangle, Nothing]

playerlist4 = [Just Square, Just Square, Just LTriangle, Nothing, Nothing,Just Square, Just Square, Just Square, Just LTriangle, Nothing, Just Square, Just Window, Just RTriangle, Nothing, Nothing, Just Square, Just RTriangle,Nothing, Nothing, Nothing]

playerlist5 = [Just Square, Just Square, Just LTriangle, Nothing, Nothing,Just Square, Just Window, Just Square, Just LTriangle, Nothing,Just Square, Just Window, Just Square, Just RTriangle, Nothing,Just Square, Just Square, Just RTriangle,Nothing,Nothing]

playerlist6 = [Just Square, Just LTriangle, Nothing, Nothing, Nothing,Just Square, Just Square, Just LTriangle, Nothing,Nothing,Just Square, Just Square, Just Square, Nothing, Nothing,Just Square, Just Window, Just RTriangle,Nothing,Nothing]

playerlist7 = [Just Square, Just Square, Just Square, Just LTriangle, Nothing, Just Square, Just Window, Just Square, Just Square, Just LTriangle, Just Square, Just Square,Just Square,Just Window, Just RTriangle, Just Square, Just Square, Just Square, Just RTriangle, Nothing]

playerlist8 = [Just Square, Just Square, Just Square, Just LTriangle, Nothing, Just Square, Just Square, Just Square, Just Window, Just LTriangle, Just Window, Just Square,Just Window,Just Square, Just RTriangle, Just Square, Just Square, Just Square, Just RTriangle, Nothing]

playerlist9 = [Just Square, Just Square, Just Square, Just LTriangle, Nothing, Just Square, Just Square, Just Square, Just Window, Just LTriangle, Just Square, Just Square,Just Square,Just Window, Just RTriangle, Just Square, Just Square, Just Square, Just RTriangle, Just Square]


solutionlist = concat $ repeat [playerlist7,playerlist1, playerlist2, playerlist8,playerlist9,playerlist3, playerlist4, playerlist5, playerlist6]

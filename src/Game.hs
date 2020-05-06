{-#Language ScopedTypeVariables#-}
module Game
where
import Data.Array
import Data.Tuple
import Data.Maybe
import System.Random
import System.Random.Shuffle

data Winning = Winning|Losing  deriving (Eq, Show)
data Shape = Square | LTriangle | RTriangle | Window deriving (Eq,Show)
type Cell = Maybe Shape
data State = Running | GameOver (Winning) deriving (Eq, Show)

type Board = Array (Int, Int) Cell
type Solution = [Cell]
type Solutionlist = [Solution]

data Game = Game { gameBoard :: Board
                 , gameShape :: Shape
                 , gameState :: State
                 , shapeList :: [Shape]
                 , gameSolution :: Board
                 , solutionShapes :: Solution
                 , nextSolutions :: Solutionlist
                 , nextRandomGen :: StdGen
                 } deriving (Show)

n :: Int
n = 5

randomtest :: IO ()
randomtest = do
  (randomnum :: Int) <-randomRIO (0,20)
  print randomnum

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

initialGame solutionshapeslist  gen = Game { gameBoard = array indexRange $ zip (range indexRange) (repeat Nothing)
                   , gameShape = head shapelist
                   , gameState = Running
                   , shapeList = tail shapelist
                   , gameSolution = (solution solutionshapes)
                   , solutionShapes = solutionshapes
                   , nextSolutions = nextsolutions
                   , nextRandomGen = nextGen gen
                   }
    where indexRange = ((0, 0), (n - 1, n - 1))
          solutionshapes = head solutionshapeslist
          nextsolutions =  tail solutionshapeslist
          shapelist = concat $repeat$solution2shapelist (currentGen gen) solutionshapes

transposed :: [(a,b)]->[(b,a)]
transposed tuplelist = map swap tuplelist

solution shapelist = array indexRange $ zip (transposed (range indexRange))(shapelist ++ repeat Nothing)
    where indexRange = ((0, 0), (n - 1, n - 1))

randomGenStd = currentGen$ mkStdGen 42 

nextGen = fst . split

currentGen = snd . split
extractfrommaybes =  (map fromJust) . (filter isJust)

solution2shapelist randomgen maybeshapelist = shuffle' shapeslist (length shapeslist) randomgen

  where shapeslist = extractfrommaybes maybeshapelist
--suggestions for building

shapelist1 = [Just Square, Just Square, Just LTriangle, Nothing, Nothing,Just Square, Just Square, Just Window, Just LTriangle, Nothing,Just Square, Just Square, Just Window, Just RTriangle, Nothing,Just Square, Just Square, Just RTriangle,Nothing,Nothing]

shapelist2 = [Just Square, Just Square, Just LTriangle, Nothing, Nothing,Just Square, Just Square, Just RTriangle, Nothing, Nothing, Just Square, Just Window, Just LTriangle, Nothing, Nothing, Just Square, Just Square, Just RTriangle, Nothing,Nothing]

shapelist3 = [Just Square, Just LTriangle, Nothing, Nothing, Nothing,Just Square, Just RTriangle, Nothing, Nothing, Nothing, Just Square, Just Square, Just Square, Just LTriangle, Nothing, Just Square, Just Square,Just Window, Just RTriangle, Nothing]

shapelist4 = [Just Square, Just Square, Just LTriangle, Nothing, Nothing,Just Square, Just Square, Just Square, Just LTriangle, Nothing, Just Square, Just Window, Just RTriangle, Nothing, Nothing, Just Square, Just RTriangle,Nothing, Nothing, Nothing]

shapelist5 = [Just Square, Just Square, Just LTriangle, Nothing, Nothing,Just Square, Just Window, Just Square, Just LTriangle, Nothing,Just Square, Just Window, Just Square, Just RTriangle, Nothing,Just Square, Just Square, Just RTriangle,Nothing,Nothing]

shapelist6 = [Just Square, Just LTriangle, Nothing, Nothing, Nothing,Just Square, Just Square, Just LTriangle, Nothing,Nothing,Just Square, Just Square, Just Square, Nothing, Nothing,Just Square, Just Window, Just RTriangle,Nothing,Nothing]

shapelist7 = [Just Square, Just Square, Just Square, Just LTriangle, Nothing, Just Square, Just Window, Just Square, Just Square, Just LTriangle, Just Square, Just Square,Just Square,Just Window, Just RTriangle, Just Square, Just Square, Just Square, Just RTriangle, Nothing]

shapelist8 = [Just Square, Just Square, Just Square, Just LTriangle, Nothing, Just Square, Just Square, Just Square, Just Window, Just LTriangle, Just Window, Just Square,Just Window,Just Square, Just RTriangle, Just Square, Just Square, Just Square, Just RTriangle, Nothing]

shapelist9 = [Just Square, Just Square, Just Square, Just LTriangle, Nothing, Just Square, Just Square, Just Square, Just Window, Just LTriangle, Just Square, Just Square,Just Square,Just Window, Just RTriangle, Just Square, Just Square, Just Square, Just RTriangle, Just Square]

shapelist10 = [Just Square, Just Square, Just Square, Just LTriangle, Nothing, Just Square, Just Square, Just Square, Just Window, Just LTriangle, Just Square, Just Square,Just Square,Just Window, Just RTriangle, Just Square, Just Square, Just Square, Just Square, Just Square]


solutionlist = concat $ repeat [shapelist7,shapelist1, shapelist2, shapelist10,shapelist9,shapelist3, shapelist4, shapelist5, shapelist6,shapelist8]
